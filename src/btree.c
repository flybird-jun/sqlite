/*
** 2004 April 6
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** This file implements an external (disk-based) database using BTrees.
** See the header comment on "btreeInt.h" for additional information.
** Including a description of file format and an overview of operation.
*/
#include "btreeInt.h"

/*
** The header string that appears at the beginning of every
** SQLite database.
*/
static const char zMagicHeader[] = SQLITE_FILE_HEADER;

/*
** Set this global variable to 1 to enable tracing using the TRACE
** macro.
*/
#if 0
int sqlite3BtreeTrace=1;  /* True to enable tracing */
# define TRACE(X)  if(sqlite3BtreeTrace){printf X;fflush(stdout);}
#else
# define TRACE(X)
#endif

/*
** Extract a 2-byte big-endian integer from an array of unsigned bytes.
** But if the value is zero, make it 65536.
**
** This routine is used to extract the "offset to cell content area" value
** from the header of a btree page.  If the page size is 65536 and the page
** is empty, the offset should be 65536, but the 2-byte value stores zero.
** This routine makes the necessary adjustment to 65536.
*/
#define get2byteNotZero(X)  (((((int)get2byte(X))-1)&0xffff)+1)

/*
** Values passed as the 5th argument to allocateBtreePage()
*/
#define BTALLOC_ANY   0           /* Allocate any page */
#define BTALLOC_EXACT 1           /* Allocate exact page if possible */
#define BTALLOC_LE    2           /* Allocate any page <= the parameter */

/*
** Macro IfNotOmitAV(x) returns (x) if SQLITE_OMIT_AUTOVACUUM is not 
** defined, or 0 if it is. For example:
**
**   bIncrVacuum = IfNotOmitAV(pBtShared->incrVacuum);
*/
#ifndef SQLITE_OMIT_AUTOVACUUM
#define IfNotOmitAV(expr) (expr)
#else
#define IfNotOmitAV(expr) 0
#endif

#ifndef SQLITE_OMIT_SHARED_CACHE
/*
** A list of BtShared objects that are eligible for participation
** in shared cache.  This variable has file scope during normal builds,
** but the test harness needs to access it so we make it global for 
** test builds.
**
** Access to this variable is protected by SQLITE_MUTEX_STATIC_MAIN.
*/
#ifdef SQLITE_TEST
BtShared *SQLITE_WSD sqlite3SharedCacheList = 0;
#else
static BtShared *SQLITE_WSD sqlite3SharedCacheList = 0;
#endif
#endif /* SQLITE_OMIT_SHARED_CACHE */

#ifndef SQLITE_OMIT_SHARED_CACHE

// 启用或禁用共享页缓存（shared pager）和模式（schema）功能。此设置对已存在的数据库连接无效。只影响新Open的数据库连接。
int sqlite3_enable_shared_cache(int enable){
  sqlite3GlobalConfig.sharedCacheEnabled = enable;
  return SQLITE_OK;
}
#endif



#ifdef SQLITE_OMIT_SHARED_CACHE
  /*
  ** The functions querySharedCacheTableLock(), setSharedCacheTableLock(),
  ** and clearAllSharedCacheTableLocks()
  ** manipulate entries in the BtShared.pLock linked list used to store
  ** shared-cache table level locks. If the library is compiled with the
  ** shared-cache feature disabled, then there is only ever one user
  ** of each BtShared structure and so this locking is not necessary. 
  ** So define the lock related functions as no-ops.
  */
  #define querySharedCacheTableLock(a,b,c) SQLITE_OK
  #define setSharedCacheTableLock(a,b,c) SQLITE_OK
  #define clearAllSharedCacheTableLocks(a)
  #define downgradeAllSharedCacheTableLocks(a)
  #define hasSharedCacheTableLock(a,b,c,d) 1
  #define hasReadConflicts(a, b) 0
#endif

#ifdef SQLITE_DEBUG
/*
** Return and reset the seek counter for a Btree object.
*/
sqlite3_uint64 sqlite3BtreeSeekCount(Btree *pBt){
  u64 n =  pBt->nSeek;
  pBt->nSeek = 0;
  return n;
}
#endif

/*
** Implementation of the SQLITE_CORRUPT_PAGE() macro. Takes a single
** (MemPage*) as an argument. The (MemPage*) must not be NULL.
**
** If SQLITE_DEBUG is not defined, then this macro is equivalent to
** SQLITE_CORRUPT_BKPT. Or, if SQLITE_DEBUG is set, then the log message
** normally produced as a side-effect of SQLITE_CORRUPT_BKPT is augmented
** with the page number and filename associated with the (MemPage*).
*/
#ifdef SQLITE_DEBUG
int corruptPageError(int lineno, MemPage *p){
  char *zMsg;
  sqlite3BeginBenignMalloc();
  zMsg = sqlite3_mprintf("database corruption page %d of %s",
      (int)p->pgno, sqlite3PagerFilename(p->pBt->pPager, 0)
  );
  sqlite3EndBenignMalloc();
  if( zMsg ){
    sqlite3ReportError(SQLITE_CORRUPT, lineno, zMsg);
  }
  sqlite3_free(zMsg);
  return SQLITE_CORRUPT_BKPT;
}
# define SQLITE_CORRUPT_PAGE(pMemPage) corruptPageError(__LINE__, pMemPage)
#else
# define SQLITE_CORRUPT_PAGE(pMemPage) SQLITE_CORRUPT_PGNO(pMemPage->pgno)
#endif

#ifndef SQLITE_OMIT_SHARED_CACHE

#ifdef SQLITE_DEBUG
/*
**** This function is only used as part of an assert() statement. ***
**
** Check to see if pBtree holds the required locks to read or write to the 
** table with root page iRoot.   Return 1 if it does and 0 if not.
**
** For example, when writing to a table with root-page iRoot via 
** Btree connection pBtree:
**
**    assert( hasSharedCacheTableLock(pBtree, iRoot, 0, WRITE_LOCK) );
**
** When writing to an index that resides in a sharable database, the 
** caller should have first obtained a lock specifying the root page of
** the corresponding table. This makes things a bit more complicated,
** as this module treats each table as a separate structure. To determine
** the table corresponding to the index being written, this
** function has to search through the database schema.
**
** Instead of a lock on the table/index rooted at page iRoot, the caller may
** hold a write-lock on the schema table (root page 1). This is also
** acceptable.
*/
static int hasSharedCacheTableLock(
  Btree *pBtree,         /* Handle that must hold lock */
  Pgno iRoot,            /* Root page of b-tree */
  int isIndex,           /* True if iRoot is the root of an index b-tree */
  int eLockType          /* Required lock type (READ_LOCK or WRITE_LOCK) */
){
  Schema *pSchema = (Schema *)pBtree->pBt->pSchema;
  Pgno iTab = 0;
  BtLock *pLock;

  /* If this database is not shareable, or if the client is reading
  ** and has the read-uncommitted flag set, then no lock is required. 
  ** Return true immediately.
  */
  if( (pBtree->sharable==0)
   || (eLockType==READ_LOCK && (pBtree->db->flags & SQLITE_ReadUncommit))
  ){
    return 1;
  }

  /* If the client is reading  or writing an index and the schema is
  ** not loaded, then it is too difficult to actually check to see if
  ** the correct locks are held.  So do not bother - just return true.
  ** This case does not come up very often anyhow.
  */
  if( isIndex && (!pSchema || (pSchema->schemaFlags&DB_SchemaLoaded)==0) ){
    return 1;
  }

  /* Figure out the root-page that the lock should be held on. For table
  ** b-trees, this is just the root page of the b-tree being read or
  ** written. For index b-trees, it is the root page of the associated
  ** table.  */
  if( isIndex ){
    HashElem *p;
    int bSeen = 0;
    for(p=sqliteHashFirst(&pSchema->idxHash); p; p=sqliteHashNext(p)){
      Index *pIdx = (Index *)sqliteHashData(p);
      if( pIdx->tnum==iRoot ){
        if( bSeen ){
          /* Two or more indexes share the same root page.  There must
          ** be imposter tables.  So just return true.  The assert is not
          ** useful in that case. */
          return 1;
        }
        iTab = pIdx->pTable->tnum;
        bSeen = 1;
      }
    }
  }else{
    iTab = iRoot;
  }

  /* Search for the required lock. Either a write-lock on root-page iTab, a 
  ** write-lock on the schema table, or (if the client is reading) a
  ** read-lock on iTab will suffice. Return 1 if any of these are found.  */
  for(pLock=pBtree->pBt->pLock; pLock; pLock=pLock->pNext){
    if( pLock->pBtree==pBtree 
     && (pLock->iTable==iTab || (pLock->eLock==WRITE_LOCK && pLock->iTable==1))
     && pLock->eLock>=eLockType 
    ){
      return 1;
    }
  }

  /* Failed to find the required lock. */
  return 0;
}
#endif /* SQLITE_DEBUG */

#ifdef SQLITE_DEBUG
/*
**** This function may be used as part of assert() statements only. ****
**
** Return true if it would be illegal for pBtree to write into the
** table or index rooted at iRoot because other shared connections are
** simultaneously reading that same table or index.
**
** It is illegal for pBtree to write if some other Btree object that
** shares the same BtShared object is currently reading or writing
** the iRoot table.  Except, if the other Btree object has the
** read-uncommitted flag set, then it is OK for the other object to
** have a read cursor.
**
** For example, before writing to any part of the table or index
** rooted at page iRoot, one should call:
**
**    assert( !hasReadConflicts(pBtree, iRoot) );
*/
static int hasReadConflicts(Btree *pBtree, Pgno iRoot){
  BtCursor *p;
  for(p=pBtree->pBt->pCursor; p; p=p->pNext){
    if( p->pgnoRoot==iRoot 
     && p->pBtree!=pBtree
     && 0==(p->pBtree->db->flags & SQLITE_ReadUncommit)
    ){
      return 1;
    }
  }
  return 0;
}
#endif    /* #ifdef SQLITE_DEBUG */

// 查询 B 树句柄 p 是否可在根页为 iTab 的表上获取类型为 eLock（READ_LOCK 或 WRITE_LOCK）的锁。允许返回SQLITE_OK，否则返回SQLITE_LOCKED 
static int querySharedCacheTableLock(Btree *p, Pgno iTab, u8 eLock){
  BtShared *pBt = p->pBt;
  BtLock *pIter;

  assert( sqlite3BtreeHoldsMutex(p) );
  assert( eLock==READ_LOCK || eLock==WRITE_LOCK );
  assert( p->db!=0 );
  assert( !(p->db->flags&SQLITE_ReadUncommit)||eLock==WRITE_LOCK||iTab==1 );
  
  // 获取写锁时必须是写者
  assert( eLock==READ_LOCK || (p==pBt->pWriter && p->inTrans==TRANS_WRITE) );
  assert( eLock==READ_LOCK || pBt->inTransaction==TRANS_WRITE );
  
  if( !p->sharable ){
    return SQLITE_OK;
  }

  if( pBt->pWriter!=p && (pBt->btsFlags & BTS_EXCLUSIVE)!=0 ){
    sqlite3ConnectionBlocked(p->db, pBt->pWriter->db);
    return SQLITE_LOCKED_SHAREDCACHE;
  }

  for(pIter=pBt->pLock; pIter; pIter=pIter->pNext){
    assert( pIter->eLock==READ_LOCK || pIter->eLock==WRITE_LOCK );
    assert( eLock==READ_LOCK || pIter->pBtree==p || pIter->eLock==READ_LOCK);
    if( pIter->pBtree!=p && pIter->iTable==iTab && pIter->eLock!=eLock ){
      sqlite3ConnectionBlocked(p->db, pIter->pBtree->db);
      if( eLock==WRITE_LOCK ){
        assert( p==pBt->pWriter );
        pBt->btsFlags |= BTS_PENDING;
      }
      return SQLITE_LOCKED_SHAREDCACHE;
    }
  }
  return SQLITE_OK;
}
#endif /* !SQLITE_OMIT_SHARED_CACHE */

#ifndef SQLITE_OMIT_SHARED_CACHE

// 用于在共享 B-Tree 结构（多连接共享同一数据库缓存）中，为 B-Tree 句柄 p 所操作的根页为 iTable 的表 添加锁（读锁或写锁）
static int setSharedCacheTableLock(Btree *p, Pgno iTable, u8 eLock){
  BtShared *pBt = p->pBt;
  BtLock *pLock = 0;
  BtLock *pIter;

  assert( sqlite3BtreeHoldsMutex(p) );
  assert( eLock==READ_LOCK || eLock==WRITE_LOCK );
  assert( p->db!=0 );


  assert( 0==(p->db->flags&SQLITE_ReadUncommit) || eLock==WRITE_LOCK );

  assert( p->sharable );
  assert( SQLITE_OK==querySharedCacheTableLock(p, iTable, eLock) );

  for(pIter=pBt->pLock; pIter; pIter=pIter->pNext){
    if( pIter->iTable==iTable && pIter->pBtree==p ){
      pLock = pIter;
      break;
    }
  }

  if( !pLock ){
    pLock = (BtLock *)sqlite3MallocZero(sizeof(BtLock));
    if( !pLock ){
      return SQLITE_NOMEM_BKPT;
    }
    pLock->iTable = iTable;
    pLock->pBtree = p;
    pLock->pNext = pBt->pLock;
    pBt->pLock = pLock;
  }

  assert( WRITE_LOCK>READ_LOCK );
  if( eLock>pLock->eLock ){
    pLock->eLock = eLock;
  }

  return SQLITE_OK;
}
#endif /* !SQLITE_OMIT_SHARED_CACHE */

#ifndef SQLITE_OMIT_SHARED_CACHE
// 清理指定连接持有的表锁，维护共享缓存一致性。
static void clearAllSharedCacheTableLocks(Btree *p){
  BtShared *pBt = p->pBt;
  BtLock **ppIter = &pBt->pLock;

  assert( sqlite3BtreeHoldsMutex(p) );
  assert( p->sharable || 0==*ppIter );
  assert( p->inTrans>0 );

  while( *ppIter ){
    BtLock *pLock = *ppIter;
    assert( (pBt->btsFlags & BTS_EXCLUSIVE)==0 || pBt->pWriter==pLock->pBtree );
    assert( pLock->pBtree->inTrans>=pLock->eLock );
    if( pLock->pBtree==p ){
      *ppIter = pLock->pNext;
      assert( pLock->iTable!=1 || pLock==&p->lock );
      if( pLock->iTable!=1 ){
        sqlite3_free(pLock);
      }
    }else{
      ppIter = &pLock->pNext;
    }
  }

  assert( (pBt->btsFlags & BTS_PENDING)==0 || pBt->pWriter );
  if( pBt->pWriter==p ){
    pBt->pWriter = 0;
    pBt->btsFlags &= ~(BTS_EXCLUSIVE|BTS_PENDING);
  }else if( pBt->nTransaction==2 ){
    /* This function is called when Btree p is concluding its 
    ** transaction. If there currently exists a writer, and p is not
    ** that writer, then the number of locks held by connections other
    ** than the writer must be about to drop to zero. In this case
    ** set the BTS_PENDING flag to 0.
    **
    ** If there is not currently a writer, then BTS_PENDING must
    ** be zero already. So this next line is harmless in that case.
    */
    pBt->btsFlags &= ~BTS_PENDING;
  }
}

/*
** This function changes all write-locks held by Btree p into read-locks.
*/
static void downgradeAllSharedCacheTableLocks(Btree *p){
  BtShared *pBt = p->pBt;
  if( pBt->pWriter==p ){
    BtLock *pLock;
    pBt->pWriter = 0;
    pBt->btsFlags &= ~(BTS_EXCLUSIVE|BTS_PENDING);
    for(pLock=pBt->pLock; pLock; pLock=pLock->pNext){
      assert( pLock->eLock==READ_LOCK || pLock->pBtree==p );
      pLock->eLock = READ_LOCK;
    }
  }
}

#endif /* SQLITE_OMIT_SHARED_CACHE */

static void releasePage(MemPage *pPage);         /* Forward reference */
static void releasePageOne(MemPage *pPage);      /* Forward reference */
static void releasePageNotNull(MemPage *pPage);  /* Forward reference */

/*
***** This routine is used inside of assert() only ****
**
** Verify that the cursor holds the mutex on its BtShared
*/
#ifdef SQLITE_DEBUG
static int cursorHoldsMutex(BtCursor *p){
  return sqlite3_mutex_held(p->pBt->mutex);
}

/* Verify that the cursor and the BtShared agree about what is the current
** database connetion. This is important in shared-cache mode. If the database 
** connection pointers get out-of-sync, it is possible for routines like
** btreeInitPage() to reference an stale connection pointer that references a
** a connection that has already closed.  This routine is used inside assert()
** statements only and for the purpose of double-checking that the btree code
** does keep the database connection pointers up-to-date.
*/
static int cursorOwnsBtShared(BtCursor *p){
  assert( cursorHoldsMutex(p) );
  return (p->pBtree->db==p->pBt->db);
}
#endif

/*
** Invalidate the overflow cache of the cursor passed as the first argument.
** on the shared btree structure pBt.
*/
#define invalidateOverflowCache(pCur) (pCur->curFlags &= ~BTCF_ValidOvfl)

/*
** Invalidate the overflow page-list cache for all cursors opened
** on the shared btree structure pBt.
*/
static void invalidateAllOverflowCache(BtShared *pBt){
  BtCursor *p;
  assert( sqlite3_mutex_held(pBt->mutex) );
  for(p=pBt->pCursor; p; p=p->pNext){
    invalidateOverflowCache(p);
  }
}

#ifndef SQLITE_OMIT_INCRBLOB
/*
** This function is called before modifying the contents of a table
** to invalidate any incrblob cursors that are open on the
** row or one of the rows being modified.
**
** If argument isClearTable is true, then the entire contents of the
** table is about to be deleted. In this case invalidate all incrblob
** cursors open on any row within the table with root-page pgnoRoot.
**
** Otherwise, if argument isClearTable is false, then the row with
** rowid iRow is being replaced or deleted. In this case invalidate
** only those incrblob cursors open on that specific row.
*/
static void invalidateIncrblobCursors(
  Btree *pBtree,          /* The database file to check */
  Pgno pgnoRoot,          /* The table that might be changing */
  i64 iRow,               /* The rowid that might be changing */
  int isClearTable        /* True if all rows are being deleted */
){
  BtCursor *p;
  assert( pBtree->hasIncrblobCur );
  assert( sqlite3BtreeHoldsMutex(pBtree) );
  pBtree->hasIncrblobCur = 0;
  for(p=pBtree->pBt->pCursor; p; p=p->pNext){
    if( (p->curFlags & BTCF_Incrblob)!=0 ){
      pBtree->hasIncrblobCur = 1;
      if( p->pgnoRoot==pgnoRoot && (isClearTable || p->info.nKey==iRow) ){
        p->eState = CURSOR_INVALID;
      }
    }
  }
}

#else
  /* Stub function when INCRBLOB is omitted */
  #define invalidateIncrblobCursors(w,x,y,z)
#endif /* SQLITE_OMIT_INCRBLOB */

/*
** Set bit pgno of the BtShared.pHasContent bitvec. This is called 
** when a page that previously contained data becomes a free-list leaf 
** page.
**
** The BtShared.pHasContent bitvec exists to work around an obscure
** bug caused by the interaction of two useful IO optimizations surrounding
** free-list leaf pages:
**
**   1) When all data is deleted from a page and the page becomes
**      a free-list leaf page, the page is not written to the database
**      (as free-list leaf pages contain no meaningful data). Sometimes
**      such a page is not even journalled (as it will not be modified,
**      why bother journalling it?).
**
**   2) When a free-list leaf page is reused, its content is not read
**      from the database or written to the journal file (why should it
**      be, if it is not at all meaningful?).
**
** By themselves, these optimizations work fine and provide a handy
** performance boost to bulk delete or insert operations. However, if
** a page is moved to the free-list and then reused within the same
** transaction, a problem comes up. If the page is not journalled when
** it is moved to the free-list and it is also not journalled when it
** is extracted from the free-list and reused, then the original data
** may be lost. In the event of a rollback, it may not be possible
** to restore the database to its original configuration.
**
** The solution is the BtShared.pHasContent bitvec. Whenever a page is 
** moved to become a free-list leaf page, the corresponding bit is
** set in the bitvec. Whenever a leaf page is extracted from the free-list,
** optimization 2 above is omitted if the corresponding bit is already
** set in BtShared.pHasContent. The contents of the bitvec are cleared
** at the end of every transaction.
*/
static int btreeSetHasContent(BtShared *pBt, Pgno pgno){
  int rc = SQLITE_OK;
  if( !pBt->pHasContent ){
    assert( pgno<=pBt->nPage );
    pBt->pHasContent = sqlite3BitvecCreate(pBt->nPage);
    if( !pBt->pHasContent ){
      rc = SQLITE_NOMEM_BKPT;
    }
  }
  if( rc==SQLITE_OK && pgno<=sqlite3BitvecSize(pBt->pHasContent) ){
    rc = sqlite3BitvecSet(pBt->pHasContent, pgno);
  }
  return rc;
}

/*
** Query the BtShared.pHasContent vector.
**
** This function is called when a free-list leaf page is removed from the
** free-list for reuse. It returns false if it is safe to retrieve the
** page from the pager layer with the 'no-content' flag set. True otherwise.
*/
static int btreeGetHasContent(BtShared *pBt, Pgno pgno){
  Bitvec *p = pBt->pHasContent;
  return p && (pgno>sqlite3BitvecSize(p) || sqlite3BitvecTestNotNull(p, pgno));
}

/*
** Clear (destroy) the BtShared.pHasContent bitvec. This should be
** invoked at the conclusion of each write-transaction.
*/
static void btreeClearHasContent(BtShared *pBt){
  sqlite3BitvecDestroy(pBt->pHasContent);
  pBt->pHasContent = 0;
}

/*
** Release all of the apPage[] pages for a cursor.
*/
static void btreeReleaseAllCursorPages(BtCursor *pCur){
  int i;
  if( pCur->iPage>=0 ){
    for(i=0; i<pCur->iPage; i++){
      releasePageNotNull(pCur->apPage[i]);
    }
    releasePageNotNull(pCur->pPage);
    pCur->iPage = -1;
  }
}

/*
** The cursor passed as the only argument must point to a valid entry
** when this function is called (i.e. have eState==CURSOR_VALID). This
** function saves the current cursor key in variables pCur->nKey and
** pCur->pKey. SQLITE_OK is returned if successful or an SQLite error 
** code otherwise.
**
** If the cursor is open on an intkey table, then the integer key
** (the rowid) is stored in pCur->nKey and pCur->pKey is left set to
** NULL. If the cursor is open on a non-intkey table, then pCur->pKey is 
** set to point to a malloced buffer pCur->nKey bytes in size containing 
** the key.
*/
static int saveCursorKey(BtCursor *pCur){
  int rc = SQLITE_OK;
  assert( CURSOR_VALID==pCur->eState );
  assert( 0==pCur->pKey );
  assert( cursorHoldsMutex(pCur) );

  if( pCur->curIntKey ){
    /* Only the rowid is required for a table btree */
    pCur->nKey = sqlite3BtreeIntegerKey(pCur);
  }else{
    /* For an index btree, save the complete key content. It is possible
    ** that the current key is corrupt. In that case, it is possible that
    ** the sqlite3VdbeRecordUnpack() function may overread the buffer by
    ** up to the size of 1 varint plus 1 8-byte value when the cursor 
    ** position is restored. Hence the 17 bytes of padding allocated 
    ** below. */
    void *pKey;
    pCur->nKey = sqlite3BtreePayloadSize(pCur);
    pKey = sqlite3Malloc( pCur->nKey + 9 + 8 );
    if( pKey ){
      rc = sqlite3BtreePayload(pCur, 0, (int)pCur->nKey, pKey);
      if( rc==SQLITE_OK ){
        memset(((u8*)pKey)+pCur->nKey, 0, 9+8);
        pCur->pKey = pKey;
      }else{
        sqlite3_free(pKey);
      }
    }else{
      rc = SQLITE_NOMEM_BKPT;
    }
  }
  assert( !pCur->curIntKey || !pCur->pKey );
  return rc;
}

/*
** Save the current cursor position in the variables BtCursor.nKey 
** and BtCursor.pKey. The cursor's state is set to CURSOR_REQUIRESEEK.
**
** The caller must ensure that the cursor is valid (has eState==CURSOR_VALID)
** prior to calling this routine.  
*/
static int saveCursorPosition(BtCursor *pCur){
  int rc;

  assert( CURSOR_VALID==pCur->eState || CURSOR_SKIPNEXT==pCur->eState );
  assert( 0==pCur->pKey );
  assert( cursorHoldsMutex(pCur) );

  if( pCur->curFlags & BTCF_Pinned ){
    return SQLITE_CONSTRAINT_PINNED;
  }
  if( pCur->eState==CURSOR_SKIPNEXT ){
    pCur->eState = CURSOR_VALID;
  }else{
    pCur->skipNext = 0;
  }

  rc = saveCursorKey(pCur);
  if( rc==SQLITE_OK ){
    btreeReleaseAllCursorPages(pCur);
    pCur->eState = CURSOR_REQUIRESEEK;
  }

  pCur->curFlags &= ~(BTCF_ValidNKey|BTCF_ValidOvfl|BTCF_AtLast);
  return rc;
}

/* Forward reference */
static int SQLITE_NOINLINE saveCursorsOnList(BtCursor*,Pgno,BtCursor*);

/*
** Save the positions of all cursors (except pExcept) that are open on
** the table with root-page iRoot.  "Saving the cursor position" means that
** the location in the btree is remembered in such a way that it can be
** moved back to the same spot after the btree has been modified.  This
** routine is called just before cursor pExcept is used to modify the
** table, for example in BtreeDelete() or BtreeInsert().
**
** If there are two or more cursors on the same btree, then all such 
** cursors should have their BTCF_Multiple flag set.  The btreeCursor()
** routine enforces that rule.  This routine only needs to be called in
** the uncommon case when pExpect has the BTCF_Multiple flag set.
**
** If pExpect!=NULL and if no other cursors are found on the same root-page,
** then the BTCF_Multiple flag on pExpect is cleared, to avoid another
** pointless call to this routine.
**
** Implementation note:  This routine merely checks to see if any cursors
** need to be saved.  It calls out to saveCursorsOnList() in the (unusual)
** event that cursors are in need to being saved.
*/
static int saveAllCursors(BtShared *pBt, Pgno iRoot, BtCursor *pExcept){
  BtCursor *p;
  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( pExcept==0 || pExcept->pBt==pBt );
  for(p=pBt->pCursor; p; p=p->pNext){
    if( p!=pExcept && (0==iRoot || p->pgnoRoot==iRoot) ) break;
  }
  if( p ) return saveCursorsOnList(p, iRoot, pExcept);
  if( pExcept ) pExcept->curFlags &= ~BTCF_Multiple;
  return SQLITE_OK;
}

/* This helper routine to saveAllCursors does the actual work of saving
** the cursors if and when a cursor is found that actually requires saving.
** The common case is that no cursors need to be saved, so this routine is
** broken out from its caller to avoid unnecessary stack pointer movement.
*/
static int SQLITE_NOINLINE saveCursorsOnList(
  BtCursor *p,         /* The first cursor that needs saving */
  Pgno iRoot,          /* Only save cursor with this iRoot. Save all if zero */
  BtCursor *pExcept    /* Do not save this cursor */
){
  do{
    if( p!=pExcept && (0==iRoot || p->pgnoRoot==iRoot) ){
      if( p->eState==CURSOR_VALID || p->eState==CURSOR_SKIPNEXT ){
        int rc = saveCursorPosition(p);
        if( SQLITE_OK!=rc ){
          return rc;
        }
      }else{
        testcase( p->iPage>=0 );
        btreeReleaseAllCursorPages(p);
      }
    }
    p = p->pNext;
  }while( p );
  return SQLITE_OK;
}

/*
** Clear the current cursor position.
*/
void sqlite3BtreeClearCursor(BtCursor *pCur){
  assert( cursorHoldsMutex(pCur) );
  sqlite3_free(pCur->pKey);
  pCur->pKey = 0;
  pCur->eState = CURSOR_INVALID;
}

/*
** 在此版本的 BtreeMoveto 函数中，参数 pKey 是一个打包的索引记录，
** 例如由 OP_MakeRecord 操作码生成的数据。该函数需先解包该记录，
** 然后调用 sqlite3BtreeIndexMoveto() 执行实际的索引定位操作。
*/
static int btreeMoveto(
  BtCursor *pCur,     /* Cursor open on the btree to be searched */
  const void *pKey,   /* Packed key if the btree is an index */
  i64 nKey,           /* Integer key for tables.  Size of pKey for indices */
  int bias,           /* Bias search to the high end */
  int *pRes           /* Write search results here */
){
  int rc;                    /* Status code */
  UnpackedRecord *pIdxKey;   /* Unpacked index key */

  if( pKey ){
    KeyInfo *pKeyInfo = pCur->pKeyInfo;
    assert( nKey==(i64)(int)nKey );
    pIdxKey = sqlite3VdbeAllocUnpackedRecord(pKeyInfo);
    if( pIdxKey==0 ) return SQLITE_NOMEM_BKPT;
    sqlite3VdbeRecordUnpack(pKeyInfo, (int)nKey, pKey, pIdxKey);
    if( pIdxKey->nField==0 || pIdxKey->nField>pKeyInfo->nAllField ){
      rc = SQLITE_CORRUPT_BKPT;
    }else{
      rc = sqlite3BtreeIndexMoveto(pCur, pIdxKey, pRes);
    }
    sqlite3DbFree(pCur->pKeyInfo->db, pIdxKey);
  }else{
    pIdxKey = 0;
    rc = sqlite3BtreeTableMoveto(pCur, nKey, bias, pRes);
  }
  return rc;
}

/*
** 将游标恢复到调用 saveCursorPosition() 时的位置（或尽可能接近的位置）。
** 注意：此调用会删除由 saveCursorPosition() 保存的位置信息，
** 因此每次 saveCursorPosition() 后最多只能执行一次有效的 restoreCursorPosition()。
*/
static int btreeRestoreCursorPosition(BtCursor *pCur){
  int rc;
  int skipNext = 0;
  assert( cursorOwnsBtShared(pCur) );
  assert( pCur->eState>=CURSOR_REQUIRESEEK );
  if( pCur->eState==CURSOR_FAULT ){
    return pCur->skipNext;
  }
  pCur->eState = CURSOR_INVALID;
  if( sqlite3FaultSim(410) ){
    rc = SQLITE_IOERR;
  }else{
    rc = btreeMoveto(pCur, pCur->pKey, pCur->nKey, 0, &skipNext);
  }
  if( rc==SQLITE_OK ){
    sqlite3_free(pCur->pKey);
    pCur->pKey = 0;
    assert( pCur->eState==CURSOR_VALID || pCur->eState==CURSOR_INVALID );
    if( skipNext ) pCur->skipNext = skipNext;
    if( pCur->skipNext && pCur->eState==CURSOR_VALID ){
      pCur->eState = CURSOR_SKIPNEXT;
    }
  }
  return rc;
}

#define restoreCursorPosition(p) \
  (p->eState>=CURSOR_REQUIRESEEK ? \
         btreeRestoreCursorPosition(p) : \
         SQLITE_OK)

/*
** Determine whether or not a cursor has moved from the position where
** it was last placed, or has been invalidated for any other reason.
** Cursors can move when the row they are pointing at is deleted out
** from under them, for example.  Cursor might also move if a btree
** is rebalanced.
**
** Calling this routine with a NULL cursor pointer returns false.
**
** Use the separate sqlite3BtreeCursorRestore() routine to restore a cursor
** back to where it ought to be if this routine returns true.
*/
int sqlite3BtreeCursorHasMoved(BtCursor *pCur){
  assert( EIGHT_BYTE_ALIGNMENT(pCur)
       || pCur==sqlite3BtreeFakeValidCursor() );
  assert( offsetof(BtCursor, eState)==0 );
  assert( sizeof(pCur->eState)==1 );
  return CURSOR_VALID != *(u8*)pCur;
}

/*
** Return a pointer to a fake BtCursor object that will always answer
** false to the sqlite3BtreeCursorHasMoved() routine above.  The fake
** cursor returned must not be used with any other Btree interface.
*/
BtCursor *sqlite3BtreeFakeValidCursor(void){
  static u8 fakeCursor = CURSOR_VALID;
  assert( offsetof(BtCursor, eState)==0 );
  return (BtCursor*)&fakeCursor;
}

/*
** This routine restores a cursor back to its original position after it
** has been moved by some outside activity (such as a btree rebalance or
** a row having been deleted out from under the cursor).  
**
** On success, the *pDifferentRow parameter is false if the cursor is left
** pointing at exactly the same row.  *pDifferntRow is the row the cursor
** was pointing to has been deleted, forcing the cursor to point to some
** nearby row.
**
** This routine should only be called for a cursor that just returned
** TRUE from sqlite3BtreeCursorHasMoved().
*/
int sqlite3BtreeCursorRestore(BtCursor *pCur, int *pDifferentRow){
  int rc;

  assert( pCur!=0 );
  assert( pCur->eState!=CURSOR_VALID );
  rc = restoreCursorPosition(pCur);
  if( rc ){
    *pDifferentRow = 1;
    return rc;
  }
  if( pCur->eState!=CURSOR_VALID ){
    *pDifferentRow = 1;
  }else{
    *pDifferentRow = 0;
  }
  return SQLITE_OK;
}

#ifdef SQLITE_ENABLE_CURSOR_HINTS
/*
** Provide hints to the cursor.  The particular hint given (and the type
** and number of the varargs parameters) is determined by the eHintType
** parameter.  See the definitions of the BTREE_HINT_* macros for details.
*/
void sqlite3BtreeCursorHint(BtCursor *pCur, int eHintType, ...){
  /* Used only by system that substitute their own storage engine */
}
#endif

/*
** Provide flag hints to the cursor.
*/
void sqlite3BtreeCursorHintFlags(BtCursor *pCur, unsigned x){
  assert( x==BTREE_SEEK_EQ || x==BTREE_BULKLOAD || x==0 );
  pCur->hints = x;
}


#ifndef SQLITE_OMIT_AUTOVACUUM
/*
** 给定一个普通数据库页的页号（pgno），返回包含该页指针映射条目的指针映射页的页号。
** 
** 若输入页号 pgno == 1，则返回 0（因为页 1 没有关联的指针映射页）。
** 完整性检查（integrity_check）逻辑要求 ptrmapPageno(*,1) 的结果必须不等于 1。
*/
static Pgno ptrmapPageno(BtShared *pBt, Pgno pgno){
  int nPagesPerMapPage;
  Pgno iPtrMap, ret;
  assert( sqlite3_mutex_held(pBt->mutex) );
  if( pgno<2 ) return 0;
  nPagesPerMapPage = (pBt->usableSize/5)+1;
  iPtrMap = (pgno-2)/nPagesPerMapPage;
  ret = (iPtrMap*nPagesPerMapPage) + 2;
  if( ret==PENDING_BYTE_PAGE(pBt) ){
    ret++;
  }
  return ret;
}

// 将一条条目写入指针映射（pointer map）。
static void ptrmapPut(BtShared *pBt, Pgno key, u8 eType, Pgno parent, int *pRC){
  DbPage *pDbPage;  /* The pointer map page */
  u8 *pPtrmap;      /* The pointer map data */
  Pgno iPtrmap;     /* The pointer map page number */
  int offset;       /* Offset in pointer map page */
  int rc;           /* Return code from subfunctions */

  if( *pRC ) return;

  assert( sqlite3_mutex_held(pBt->mutex) );
  /* The super-journal page number must never be used as a pointer map page */
  assert( 0==PTRMAP_ISPAGE(pBt, PENDING_BYTE_PAGE(pBt)) );

  assert( pBt->autoVacuum );
  if( key==0 ){
    *pRC = SQLITE_CORRUPT_BKPT;
    return;
  }
  iPtrmap = PTRMAP_PAGENO(pBt, key);
  rc = sqlite3PagerGet(pBt->pPager, iPtrmap, &pDbPage, 0);
  if( rc!=SQLITE_OK ){
    *pRC = rc;
    return;
  }
  if( ((char*)sqlite3PagerGetExtra(pDbPage))[0]!=0 ){
    /* The first byte of the extra data is the MemPage.isInit byte.
    ** If that byte is set, it means this page is also being used
    ** as a btree page. */
    *pRC = SQLITE_CORRUPT_BKPT;
    goto ptrmap_exit;
  }
  offset = PTRMAP_PTROFFSET(iPtrmap, key);
  if( offset<0 ){
    *pRC = SQLITE_CORRUPT_BKPT;
    goto ptrmap_exit;
  }
  assert( offset <= (int)pBt->usableSize-5 );
  pPtrmap = (u8 *)sqlite3PagerGetData(pDbPage);

  if( eType!=pPtrmap[offset] || get4byte(&pPtrmap[offset+1])!=parent ){
    TRACE(("PTRMAP_UPDATE: %d->(%d,%d)\n", key, eType, parent));
    *pRC= rc = sqlite3PagerWrite(pDbPage);
    if( rc==SQLITE_OK ){
      pPtrmap[offset] = eType;
      put4byte(&pPtrmap[offset+1], parent);
    }
  }

ptrmap_exit:
  sqlite3PagerUnref(pDbPage);
}

/*
** Read an entry from the pointer map.
**
** This routine retrieves the pointer map entry for page 'key', writing
** the type and parent page number to *pEType and *pPgno respectively.
** An error code is returned if something goes wrong, otherwise SQLITE_OK.
*/
static int ptrmapGet(BtShared *pBt, Pgno key, u8 *pEType, Pgno *pPgno){
  DbPage *pDbPage;   /* The pointer map page */
  int iPtrmap;       /* Pointer map page index */
  u8 *pPtrmap;       /* Pointer map page data */
  int offset;        /* Offset of entry in pointer map */
  int rc;

  assert( sqlite3_mutex_held(pBt->mutex) );

  iPtrmap = PTRMAP_PAGENO(pBt, key);
  rc = sqlite3PagerGet(pBt->pPager, iPtrmap, &pDbPage, 0);
  if( rc!=0 ){
    return rc;
  }
  pPtrmap = (u8 *)sqlite3PagerGetData(pDbPage);

  offset = PTRMAP_PTROFFSET(iPtrmap, key);
  if( offset<0 ){
    sqlite3PagerUnref(pDbPage);
    return SQLITE_CORRUPT_BKPT;
  }
  assert( offset <= (int)pBt->usableSize-5 );
  assert( pEType!=0 );
  *pEType = pPtrmap[offset];
  if( pPgno ) *pPgno = get4byte(&pPtrmap[offset+1]);

  sqlite3PagerUnref(pDbPage);
  if( *pEType<1 || *pEType>5 ) return SQLITE_CORRUPT_PGNO(iPtrmap);
  return SQLITE_OK;
}

#else /* if defined SQLITE_OMIT_AUTOVACUUM */
  #define ptrmapPut(w,x,y,z,rc)
  #define ptrmapGet(w,x,y,z) SQLITE_OK
  #define ptrmapPutOvflPtr(x, y, z, rc)
#endif

// 给定一个B树页面和一个单元格索引（0表示页面上的第一个单元格，1表示第二个，依此类推），返回指向单元格内容的指针。
#define findCell(P,I) \
  ((P)->aData + ((P)->maskPage & get2byteAligned(&(P)->aCellIdx[2*(I)])))
#define findCellPastPtr(P,I) \
  ((P)->aDataOfst + ((P)->maskPage & get2byteAligned(&(P)->aCellIdx[2*(I)])))


/*
** 此逻辑是 btreeParseCellPtr() 和 btreeParseCellPtrIndex() 的公共尾部处理部分，
** 用于处理单元格（Cell）无法完全存放在单个 B 树页（Page）中的情况。
** 在此场景下，需对 CellInfo 结构进行必要的调整。
*/
static SQLITE_NOINLINE void btreeParseCellAdjustSizeForOverflow(
  MemPage *pPage,         /* Page containing the cell */
  u8 *pCell,              /* Pointer to the cell text. */
  CellInfo *pInfo         /* Fill in this structure */
){
  /* 
  ** 如果负载（Payload）无法完全存储在本地页（Local Page）中，
  ** 则需决定将多少数据保留在本地页，以及将多少数据溢出（Spill）到溢出页（Overflow Page）。
  ** 其策略是：在保持本地存储量介于 minLocal 和 maxLocal 之间的前提下，
  ** 尽量减少溢出页上的未使用空间。
  ** 
  ** 警告：以任何方式修改溢出负载的分布策略，都将导致文件格式不兼容。
  */
  int minLocal;  /* Minimum amount of payload held locally */
  int maxLocal;  /* Maximum amount of payload held locally */
  int surplus;   /* Overflow payload available for local storage */

  minLocal = pPage->minLocal;
  maxLocal = pPage->maxLocal;
  surplus = minLocal + (pInfo->nPayload - minLocal)%(pPage->pBt->usableSize-4);
  testcase( surplus==maxLocal );
  testcase( surplus==maxLocal+1 );
  if( surplus <= maxLocal ){
    pInfo->nLocal = (u16)surplus;
  }else{
    pInfo->nLocal = (u16)minLocal;
  }
  pInfo->nSize = (u16)(&pInfo->pPayload[pInfo->nLocal] - pCell) + 4;
}

/*
** Given a record with nPayload bytes of payload stored within btree
** page pPage, return the number of bytes of payload stored locally.
*/
static int btreePayloadToLocal(MemPage *pPage, i64 nPayload){
  int maxLocal;  /* Maximum amount of payload held locally */
  maxLocal = pPage->maxLocal;
  if( nPayload<=maxLocal ){
    return nPayload;
  }else{
    int minLocal;  /* Minimum amount of payload held locally */
    int surplus;   /* Overflow payload available for local storage */
    minLocal = pPage->minLocal;
    surplus = minLocal + (nPayload - minLocal)%(pPage->pBt->usableSize-4);
    return ( surplus <= maxLocal ) ? surplus : minLocal;
  }
}

/*
** The following routines are implementations of the MemPage.xParseCell()
** method.
**
** Parse a cell content block and fill in the CellInfo structure.
**
** btreeParseCellPtr()        =>   table btree leaf nodes
** btreeParseCellNoPayload()  =>   table btree internal nodes
** btreeParseCellPtrIndex()   =>   index btree nodes
**
** There is also a wrapper function btreeParseCell() that works for
** all MemPage types and that references the cell by index rather than
** by pointer.
*/
static void btreeParseCellPtrNoPayload(
  MemPage *pPage,         /* Page containing the cell */
  u8 *pCell,              /* Pointer to the cell text. */
  CellInfo *pInfo         /* Fill in this structure */
){
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( pPage->leaf==0 );
  assert( pPage->childPtrSize==4 );
#ifndef SQLITE_DEBUG
  UNUSED_PARAMETER(pPage);
#endif
  pInfo->nSize = 4 + getVarint(&pCell[4], (u64*)&pInfo->nKey);
  pInfo->nPayload = 0;
  pInfo->nLocal = 0;
  pInfo->pPayload = 0;
  return;
}
static void btreeParseCellPtr(
  MemPage *pPage,         /* Page containing the cell */
  u8 *pCell,              /* Pointer to the cell text. */
  CellInfo *pInfo         /* Fill in this structure */
){
  u8 *pIter;              /* For scanning through pCell */
  u32 nPayload;           /* Number of bytes of cell payload */
  u64 iKey;               /* Extracted Key value */

  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( pPage->leaf==0 || pPage->leaf==1 );
  assert( pPage->intKeyLeaf );
  assert( pPage->childPtrSize==0 );
  pIter = pCell;

  /* The next block of code is equivalent to:
  **
  **     pIter += getVarint32(pIter, nPayload);
  **
  ** The code is inlined to avoid a function call.
  */
  nPayload = *pIter;
  if( nPayload>=0x80 ){
    u8 *pEnd = &pIter[8];
    nPayload &= 0x7f;
    do{
      nPayload = (nPayload<<7) | (*++pIter & 0x7f);
    }while( (*pIter)>=0x80 && pIter<pEnd );
  }
  pIter++;

  /* The next block of code is equivalent to:
  **
  **     pIter += getVarint(pIter, (u64*)&pInfo->nKey);
  **
  ** The code is inlined and the loop is unrolled for performance.
  ** This routine is a high-runner.
  */
  iKey = *pIter;
  if( iKey>=0x80 ){
    u8 x;
    iKey = ((iKey&0x7f)<<7) | ((x = *++pIter) & 0x7f);
    if( x>=0x80 ){
      iKey = (iKey<<7) | ((x =*++pIter) & 0x7f);
      if( x>=0x80 ){
        iKey = (iKey<<7) | ((x = *++pIter) & 0x7f);
        if( x>=0x80 ){
          iKey = (iKey<<7) | ((x = *++pIter) & 0x7f);
          if( x>=0x80 ){
            iKey = (iKey<<7) | ((x = *++pIter) & 0x7f);
            if( x>=0x80 ){
              iKey = (iKey<<7) | ((x = *++pIter) & 0x7f);
              if( x>=0x80 ){
                iKey = (iKey<<7) | ((x = *++pIter) & 0x7f);
                if( x>=0x80 ){
                  iKey = (iKey<<8) | (*++pIter);
                }
              }
            }
          }
        }
      }
    }
  }
  pIter++;

  pInfo->nKey = *(i64*)&iKey;
  pInfo->nPayload = nPayload;
  pInfo->pPayload = pIter;
  testcase( nPayload==pPage->maxLocal );
  testcase( nPayload==(u32)pPage->maxLocal+1 );
  if( nPayload<=pPage->maxLocal ){
    /* This is the (easy) common case where the entire payload fits
    ** on the local page.  No overflow is required.
    */
    pInfo->nSize = nPayload + (u16)(pIter - pCell);
    if( pInfo->nSize<4 ) pInfo->nSize = 4;
    pInfo->nLocal = (u16)nPayload;
  }else{
    btreeParseCellAdjustSizeForOverflow(pPage, pCell, pInfo);
  }
}
static void btreeParseCellPtrIndex(
  MemPage *pPage,         /* Page containing the cell */
  u8 *pCell,              /* Pointer to the cell text. */
  CellInfo *pInfo         /* Fill in this structure */
){
  u8 *pIter;              /* For scanning through pCell */
  u32 nPayload;           /* Number of bytes of cell payload */

  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( pPage->leaf==0 || pPage->leaf==1 );
  assert( pPage->intKeyLeaf==0 );
  pIter = pCell + pPage->childPtrSize;
  nPayload = *pIter;
  if( nPayload>=0x80 ){
    u8 *pEnd = &pIter[8];
    nPayload &= 0x7f;
    do{
      nPayload = (nPayload<<7) | (*++pIter & 0x7f);
    }while( *(pIter)>=0x80 && pIter<pEnd );
  }
  pIter++;
  pInfo->nKey = nPayload;
  pInfo->nPayload = nPayload;
  pInfo->pPayload = pIter;
  testcase( nPayload==pPage->maxLocal );
  testcase( nPayload==(u32)pPage->maxLocal+1 );
  if( nPayload<=pPage->maxLocal ){
    pInfo->nSize = nPayload + (u16)(pIter - pCell);
    if( pInfo->nSize<4 ) pInfo->nSize = 4;
    pInfo->nLocal = (u16)nPayload;
  }else{
    btreeParseCellAdjustSizeForOverflow(pPage, pCell, pInfo);
  }
}
static void btreeParseCell(
  MemPage *pPage,         /* Page containing the cell */
  int iCell,              /* The cell index.  First cell is 0 */
  CellInfo *pInfo         /* Fill in this structure */
){
  pPage->xParseCell(pPage, findCell(pPage, iCell), pInfo);
}

/*
** The following routines are implementations of the MemPage.xCellSize
** method.
**
** Compute the total number of bytes that a Cell needs in the cell
** data area of the btree-page.  The return number includes the cell
** data header and the local payload, but not any overflow page or
** the space used by the cell pointer.
**
** cellSizePtrNoPayload()    =>   table internal nodes
** cellSizePtrTableLeaf()    =>   table leaf nodes
** cellSizePtr()             =>   all index nodes & table leaf nodes
*/
static u16 cellSizePtr(MemPage *pPage, u8 *pCell){
  u8 *pIter = pCell + pPage->childPtrSize; /* For looping over bytes of pCell */
  u8 *pEnd;                                /* End mark for a varint */
  u32 nSize;                               /* Size value to return */

#ifdef SQLITE_DEBUG
  /* The value returned by this function should always be the same as
  ** the (CellInfo.nSize) value found by doing a full parse of the
  ** cell. If SQLITE_DEBUG is defined, an assert() at the bottom of
  ** this function verifies that this invariant is not violated. */
  CellInfo debuginfo;
  pPage->xParseCell(pPage, pCell, &debuginfo);
#endif

  nSize = *pIter;
  if( nSize>=0x80 ){
    pEnd = &pIter[8];
    nSize &= 0x7f;
    do{
      nSize = (nSize<<7) | (*++pIter & 0x7f);
    }while( *(pIter)>=0x80 && pIter<pEnd );
  }
  pIter++;
  testcase( nSize==pPage->maxLocal );
  testcase( nSize==(u32)pPage->maxLocal+1 );
  if( nSize<=pPage->maxLocal ){
    nSize += (u32)(pIter - pCell);
    if( nSize<4 ) nSize = 4;
  }else{
    int minLocal = pPage->minLocal;
    nSize = minLocal + (nSize - minLocal) % (pPage->pBt->usableSize - 4);
    testcase( nSize==pPage->maxLocal );
    testcase( nSize==(u32)pPage->maxLocal+1 );
    if( nSize>pPage->maxLocal ){
      nSize = minLocal;
    }
    nSize += 4 + (u16)(pIter - pCell);
  }
  assert( nSize==debuginfo.nSize || CORRUPT_DB );
  return (u16)nSize;
}
static u16 cellSizePtrNoPayload(MemPage *pPage, u8 *pCell){
  u8 *pIter = pCell + 4; /* For looping over bytes of pCell */
  u8 *pEnd;              /* End mark for a varint */

#ifdef SQLITE_DEBUG
  /* The value returned by this function should always be the same as
  ** the (CellInfo.nSize) value found by doing a full parse of the
  ** cell. If SQLITE_DEBUG is defined, an assert() at the bottom of
  ** this function verifies that this invariant is not violated. */
  CellInfo debuginfo;
  pPage->xParseCell(pPage, pCell, &debuginfo);
#else
  UNUSED_PARAMETER(pPage);
#endif

  assert( pPage->childPtrSize==4 );
  pEnd = pIter + 9;
  while( (*pIter++)&0x80 && pIter<pEnd );
  assert( debuginfo.nSize==(u16)(pIter - pCell) || CORRUPT_DB );
  return (u16)(pIter - pCell);
}
static u16 cellSizePtrTableLeaf(MemPage *pPage, u8 *pCell){
  u8 *pIter = pCell;   /* For looping over bytes of pCell */
  u8 *pEnd;            /* End mark for a varint */
  u32 nSize;           /* Size value to return */

#ifdef SQLITE_DEBUG
  /* The value returned by this function should always be the same as
  ** the (CellInfo.nSize) value found by doing a full parse of the
  ** cell. If SQLITE_DEBUG is defined, an assert() at the bottom of
  ** this function verifies that this invariant is not violated. */
  CellInfo debuginfo;
  pPage->xParseCell(pPage, pCell, &debuginfo);
#endif

  nSize = *pIter;
  if( nSize>=0x80 ){
    pEnd = &pIter[8];
    nSize &= 0x7f;
    do{
      nSize = (nSize<<7) | (*++pIter & 0x7f);
    }while( *(pIter)>=0x80 && pIter<pEnd );
  }
  pIter++;
  /* pIter now points at the 64-bit integer key value, a variable length 
  ** integer. The following block moves pIter to point at the first byte
  ** past the end of the key value. */
  if( (*pIter++)&0x80
   && (*pIter++)&0x80
   && (*pIter++)&0x80
   && (*pIter++)&0x80
   && (*pIter++)&0x80
   && (*pIter++)&0x80
   && (*pIter++)&0x80
   && (*pIter++)&0x80 ){ pIter++; }
  testcase( nSize==pPage->maxLocal );
  testcase( nSize==(u32)pPage->maxLocal+1 );
  if( nSize<=pPage->maxLocal ){
    nSize += (u32)(pIter - pCell);
    if( nSize<4 ) nSize = 4;
  }else{
    int minLocal = pPage->minLocal;
    nSize = minLocal + (nSize - minLocal) % (pPage->pBt->usableSize - 4);
    testcase( nSize==pPage->maxLocal );
    testcase( nSize==(u32)pPage->maxLocal+1 );
    if( nSize>pPage->maxLocal ){
      nSize = minLocal;
    }
    nSize += 4 + (u16)(pIter - pCell);
  }
  assert( nSize==debuginfo.nSize || CORRUPT_DB );
  return (u16)nSize;
}


#ifdef SQLITE_DEBUG
/* This variation on cellSizePtr() is used inside of assert() statements
** only. */
static u16 cellSize(MemPage *pPage, int iCell){
  return pPage->xCellSize(pPage, findCell(pPage, iCell));
}
#endif

#ifndef SQLITE_OMIT_AUTOVACUUM
/*
** The cell pCell is currently part of page pSrc but will ultimately be part
** of pPage.  (pSrc and pPage are often the same.)  If pCell contains a
** pointer to an overflow page, insert an entry into the pointer-map for
** the overflow page that will be valid after pCell has been moved to pPage.
*/
static void ptrmapPutOvflPtr(MemPage *pPage, MemPage *pSrc, u8 *pCell,int *pRC){
  CellInfo info;
  if( *pRC ) return;
  assert( pCell!=0 );
  pPage->xParseCell(pPage, pCell, &info);
  if( info.nLocal<info.nPayload ){
    Pgno ovfl;
    if( SQLITE_WITHIN(pSrc->aDataEnd, pCell, pCell+info.nLocal) ){
      testcase( pSrc!=pPage );
      *pRC = SQLITE_CORRUPT_BKPT;
      return;
    }
    ovfl = get4byte(&pCell[info.nSize-4]);
    ptrmapPut(pPage->pBt, ovfl, PTRMAP_OVERFLOW1, pPage->pgno, pRC);
  }
}
#endif


/*
** 对给定页面进行碎片整理。此例程会重新组织页面内的单元格，
** 使得空闲块列表中不再存在任何空闲块。
**
** 参数nMaxFrag指定了此函数返回后页面内允许存在的最大碎片空间量。
**
** 证据编号：R-44582-60138 SQLite可能会不时地重组B树页面，使得：
** - 不存在空闲块或碎片字节
** - 所有未使用字节都包含在未分配空间区域
** - 所有单元格都紧密排列在页面末尾
*/
static int defragmentPage(MemPage *pPage, int nMaxFrag){
  int i;                     /* Loop counter */
  int pc;                    /* Address of the i-th cell */
  int hdr;                   /* Offset to the page header */
  int size;                  /* Size of a cell */
  int usableSize;            /* Number of usable bytes on a page */
  int cellOffset;            /* Offset to the cell pointer array */
  int cbrk;                  /* Offset to the cell content area */
  int nCell;                 /* Number of cells on the page */
  unsigned char *data;       /* The page data */
  unsigned char *temp;       /* Temp area for cell content */
  unsigned char *src;        /* Source of content */
  int iCellFirst;            /* First allowable cell index */
  int iCellLast;             /* Last possible cell index */
  int iCellStart;            /* First cell offset in input */

  assert( sqlite3PagerIswriteable(pPage->pDbPage) );
  assert( pPage->pBt!=0 );
  assert( pPage->pBt->usableSize <= SQLITE_MAX_PAGE_SIZE );
  assert( pPage->nOverflow==0 );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  data = pPage->aData;
  hdr = pPage->hdrOffset;
  cellOffset = pPage->cellOffset;
  nCell = pPage->nCell;
  assert( nCell==get2byte(&data[hdr+3]) || CORRUPT_DB );
  iCellFirst = cellOffset + 2*nCell;
  usableSize = pPage->pBt->usableSize;

  /*
  ** 该代码块处理满足以下条件的页面：
  ** - 包含两个或更少空闲块
  ** - 碎片字节数不超过nMaxFrag
  **
  ** 在此情况下，相较于重构整个页面，使用memmove()移动单元格块（两个或一个）
  ** 并给单元格指针数组中的每个指针添加必要偏移量的操作效率更高。
  */
  if( (int)data[hdr+7]<=nMaxFrag ){
    int iFree = get2byte(&data[hdr+1]);
    if( iFree ){
      int iFree2 = get2byte(&data[iFree]);
      if( 0==iFree2 || (data[iFree2]==0 && data[iFree2+1]==0) ){
        u8 *pEnd = &data[cellOffset + nCell*2];
        u8 *pAddr;
        int sz2 = 0;
        int sz = get2byte(&data[iFree+2]); // 第一个空闲块大小
        int top = get2byte(&data[hdr+5]); // CellStart
        if( iFree2 ){
          sz2 = get2byte(&data[iFree2+2]);
          memmove(&data[iFree+sz+sz2], &data[iFree+sz], iFree2-(iFree+sz));
          sz += sz2;
        }
        cbrk = top+sz;
        memmove(&data[cbrk], &data[top], iFree-top);
        for(pAddr=&data[cellOffset]; pAddr<pEnd; pAddr+=2){
          pc = get2byte(pAddr);
          if( pc<iFree ){ put2byte(pAddr, pc+sz); }
          else if( pc<iFree2 ){ put2byte(pAddr, pc+sz2); }
        }
        goto defragment_out;
      }
    }
  }

  cbrk = usableSize;
  iCellLast = usableSize - 4;
  iCellStart = get2byte(&data[hdr+5]);
  if( nCell>0 ){
    temp = sqlite3PagerTempSpace(pPage->pBt->pPager);
    memcpy(&temp[iCellStart], &data[iCellStart], usableSize - iCellStart);
    src = temp;
    for(i=0; i<nCell; i++){
      u8 *pAddr;     /* The i-th cell pointer */
      pAddr = &data[cellOffset + i*2];
      pc = get2byte(pAddr); // slot->offset
      size = pPage->xCellSize(pPage, &src[pc]);
      cbrk -= size;
      put2byte(pAddr, cbrk);
      memcpy(&data[cbrk], &src[pc], size);
    }
  }
  data[hdr+7] = 0;

defragment_out:
  assert( pPage->nFree>=0 );
  if( data[hdr+7]+cbrk-iCellFirst!=pPage->nFree ){
    return SQLITE_CORRUPT_PAGE(pPage);
  }
  assert( cbrk>=iCellFirst );
  put2byte(&data[hdr+5], cbrk);
  data[hdr+1] = 0;
  data[hdr+2] = 0;
  memset(&data[iCellFirst], 0, cbrk-iCellFirst);
  assert( sqlite3PagerIswriteable(pPage->pDbPage) );
  return SQLITE_OK;
}

/*
** Search the free-list on page pPg for space to store a cell nByte bytes in
** size. If one can be found, return a pointer to the space and remove it
** from the free-list.
**
** If no suitable space can be found on the free-list, return NULL.
**
** This function may detect corruption within pPg.  If corruption is
** detected then *pRc is set to SQLITE_CORRUPT and NULL is returned.
**
** Slots on the free list that are between 1 and 3 bytes larger than nByte
** will be ignored if adding the extra space to the fragmentation count
** causes the fragmentation count to exceed 60.
*/
static u8 *pageFindSlot(MemPage *pPg, int nByte, int *pRc){
  const int hdr = pPg->hdrOffset;            /* Offset to page header */
  u8 * const aData = pPg->aData;             /* Page data */
  int iAddr = hdr + 1;                       /* Address of ptr to pc */
  u8 *pTmp = &aData[iAddr];                  /* Temporary ptr into aData[] */
  int pc = get2byte(pTmp);                    // 第一个空闲块的起始偏移
  int x;                                     /* Excess size of the slot */
  int maxPC = pPg->pBt->usableSize - nByte;  /* Max address for a usable slot */
  int size;                                  /* Size of the free slot */

  assert( pc>0 );
  while( pc<=maxPC ){
    /* EVIDENCE-OF: R-22710-53328 The third and fourth bytes of each
    ** freeblock form a big-endian integer which is the size of the freeblock
    ** in bytes, including the 4-byte header. */
    pTmp = &aData[pc+2];
    size = get2byte(pTmp);
    if( (x = size - nByte)>=0 ){
      testcase( x==4 );
      testcase( x==3 );
      if( x<4 ){
        /* EVIDENCE-OF: R-11498-58022 In a well-formed b-tree page, the total
        ** number of bytes in fragments may not exceed 60. */
        if( aData[hdr+7]>57 ) return 0;

        /* Remove the slot from the free-list. Update the number of
        ** fragmented bytes within the page. */
        memcpy(&aData[iAddr], &aData[pc], 2);
        aData[hdr+7] += (u8)x;
        return &aData[pc];
      }else if( x+pc > maxPC ){
        /* This slot extends off the end of the usable part of the page */
        *pRc = SQLITE_CORRUPT_PAGE(pPg);
        return 0;
      }else{
        // 空闲块大小节余数量大于等于4，将剩余空闲块继续使用
        put2byte(&aData[pc+2], x);
      }
      return &aData[pc + x];
    }
    iAddr = pc;
    pTmp = &aData[pc];
    pc = get2byte(pTmp);
    if( pc<=iAddr ){
      if( pc ){
        /* The next slot in the chain comes before the current slot */
        *pRc = SQLITE_CORRUPT_PAGE(pPg);
      }
      return 0;
    }
  }
  if( pc>maxPC+nByte-4 ){
    /* The free slot chain extends off the end of the page */
    *pRc = SQLITE_CORRUPT_PAGE(pPg);
  }
  return 0;
}

/*
从传入的B树页面（第一个参数）中分配nByte字节空间 Write into *pIdx the index into pPage->aData[]
** of the first byte of allocated space. Return either SQLITE_OK or
** an error code (usually SQLITE_CORRUPT).
**
** The caller guarantees that there is sufficient space to make the
** allocation.  This routine might need to defragment in order to bring
** all the space together, however.  This routine will avoid using
** the first two bytes past the cell pointer area since presumably this
** allocation is being made in order to insert a new cell, so we will
** also end up needing a new cell pointer.
pIdx 返回的是申请的内存地址（Cell地址）
*/
static int allocateSpace(MemPage *pPage, int nByte, int *pIdx){
  const int hdr = pPage->hdrOffset;    /* Local cache of pPage->hdrOffset */
  u8 * const data = pPage->aData;      /* Local cache of pPage->aData */
  int top;                             /* First byte of cell content area */
  int rc = SQLITE_OK;                  /* Integer return code */
  u8 *pTmp;                            /* Temp ptr into data[] */
  int gap;        /* First byte of gap between cell pointers and cell content */
  
  assert( sqlite3PagerIswriteable(pPage->pDbPage) );
  assert( pPage->pBt );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( nByte>=0 );  /* Minimum cell size is 4 */
  assert( pPage->nFree>=nByte );
  assert( pPage->nOverflow==0 );
  assert( nByte < (int)(pPage->pBt->usableSize-8) );

  assert( pPage->cellOffset == hdr + 12 - 4*pPage->leaf );
  gap = pPage->cellOffset + 2*pPage->nCell; // Cell数组后的字节偏移
  assert( gap<=65536 );
  /* EVIDENCE-OF: R-29356-02391 If the database uses a 65536-byte page size
  ** and the reserved space is zero (the usual value for reserved space)
  ** then the cell content offset of an empty page wants to be 65536.
  ** However, that integer is too large to be stored in a 2-byte unsigned
  ** integer, so a value of 0 is used in its place. */
  pTmp = &data[hdr+5];
  top = get2byte(pTmp); //  单元格内容区域的起始字节偏移量
  assert( top<=(int)pPage->pBt->usableSize ); /* by btreeComputeFreeSpace() */
  if( gap>top ){
    if( top==0 && pPage->pBt->usableSize==65536 ){
      top = 65536;
    }else{
      return SQLITE_CORRUPT_PAGE(pPage);
    }
  }

  /* If there is enough space between gap and top for one more cell pointer,
  ** and if the freelist is not empty, then search the
  ** freelist looking for a slot big enough to satisfy the request.
  */
  testcase( gap+2==top );
  testcase( gap+1==top );
  testcase( gap==top );
  if( (data[hdr+2] || data[hdr+1]) && gap+2<=top ){
    u8 *pSpace = pageFindSlot(pPage, nByte, &rc);
    if( pSpace ){
      int g2;
      assert( pSpace+nByte<=data+pPage->pBt->usableSize );
      *pIdx = g2 = (int)(pSpace-data);
      if( g2<=gap ){
        return SQLITE_CORRUPT_PAGE(pPage);
      }else{
        return SQLITE_OK;
      }
    }else if( rc ){
      return rc;
    }
  }

  /* The request could not be fulfilled using a freelist slot.  Check
  ** to see if defragmentation is necessary.
  */
  testcase( gap+2+nByte==top );
  if( gap+2+nByte>top ){
    assert( pPage->nCell>0 || CORRUPT_DB );
    assert( pPage->nFree>=0 );
    rc = defragmentPage(pPage, MIN(4, pPage->nFree - (2+nByte)));
    if( rc ) return rc;
    top = get2byteNotZero(&data[hdr+5]);
    assert( gap+2+nByte<=top );
  }


  /* Allocate memory from the gap in between the cell pointer array
  ** and the cell content area.  The btreeComputeFreeSpace() call has already
  ** validated the freelist.  Given that the freelist is valid, there
  ** is no way that the allocation can extend off the end of the page.
  ** The assert() below verifies the previous sentence.
  */
  top -= nByte;
  put2byte(&data[hdr+5], top);
  assert( top+nByte <= (int)pPage->pBt->usableSize );
  *pIdx = top;
  return SQLITE_OK;
}

/*
** 将 pPage->aData 中的一段数据区域归还给空闲列表（Freelist）。
** 新空闲块的起始地址为 pPage->aData[iStart]，大小为 iSize 字节。
** 
** 相邻的空闲块会被合并（Coalesced）。
** 
** 尽管空闲块列表已通过 btreeComputeFreeSpace() 检查，
** 但该函数无法检测以下问题：
** 1. 单元格（Cell）与空闲块之间的重叠；
** 2. 单元格或空闲块侵占页面尾部的预留空间。
** 因此，在此函数内部需执行额外的损坏检查，若发现问题则返回 SQLITE_CORRUPT。
*/
static int freeSpace(MemPage *pPage, u16 iStart, u16 iSize){
  u16 iPtr;                             /* Address of ptr to next freeblock */
  u16 iFreeBlk;                         /* Address of the next freeblock */
  u8 hdr;                               /* Page header size.  0 or 100 */
  u8 nFrag = 0;                         /* Reduction in fragmentation */
  u16 iOrigSize = iSize;                /* Original value of iSize */
  u16 x;                                /* Offset to cell content area */
  u32 iEnd = iStart + iSize;            /* First byte past the iStart buffer */
  unsigned char *data = pPage->aData;   /* Page content */
  u8 *pTmp;                             /* Temporary ptr into data[] */

  assert( pPage->pBt!=0 );
  assert( sqlite3PagerIswriteable(pPage->pDbPage) );
  assert( CORRUPT_DB || iStart>=pPage->hdrOffset+6+pPage->childPtrSize );
  assert( CORRUPT_DB || iEnd <= pPage->pBt->usableSize );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( iSize>=4 );   /* Minimum cell size is 4 */
  assert( iStart<=pPage->pBt->usableSize-4 );

 // 空闲块链表以升序排序，查找链表找到需要插入的位置
  hdr = pPage->hdrOffset;
  iPtr = hdr + 1;
  if( data[iPtr+1]==0 && data[iPtr]==0 ){
    iFreeBlk = 0;  /* Shortcut for the case when the freelist is empty */
  }else{
    while( (iFreeBlk = get2byte(&data[iPtr]))<iStart ){
      if( iFreeBlk<=iPtr ){
        if( iFreeBlk==0 ) break; /* TH3: corrupt082.100 */
        return SQLITE_CORRUPT_PAGE(pPage);
      }
      iPtr = iFreeBlk;
    }
    if( iFreeBlk>pPage->pBt->usableSize-4 ){ /* TH3: corrupt081.100 */
      return SQLITE_CORRUPT_PAGE(pPage);
    }
    assert( iFreeBlk>iPtr || iFreeBlk==0 || CORRUPT_DB );
  
    /* At this point:
    **    iFreeBlk:   First freeblock after iStart, or zero if none
    **    iPtr:       The address of a pointer to iFreeBlk
    **
    ** Check to see if iFreeBlk should be coalesced onto the end of iStart.
    */
    if( iFreeBlk && iEnd+3>=iFreeBlk ){
      nFrag = iFreeBlk - iEnd;
      if( iEnd>iFreeBlk ) return SQLITE_CORRUPT_PAGE(pPage);
      iEnd = iFreeBlk + get2byte(&data[iFreeBlk+2]);
      if( iEnd > pPage->pBt->usableSize ){
        return SQLITE_CORRUPT_PAGE(pPage);
      }
      iSize = iEnd - iStart;
      iFreeBlk = get2byte(&data[iFreeBlk]);
    }
  
    /* If iPtr is another freeblock (that is, if iPtr is not the freelist
    ** pointer in the page header) then check to see if iStart should be
    ** coalesced onto the end of iPtr.
    */
    if( iPtr>hdr+1 ){
      int iPtrEnd = iPtr + get2byte(&data[iPtr+2]);
      if( iPtrEnd+3>=iStart ){
        if( iPtrEnd>iStart ) return SQLITE_CORRUPT_PAGE(pPage);
        nFrag += iStart - iPtrEnd;
        iSize = iEnd - iPtr;
        iStart = iPtr;
      }
    }
    if( nFrag>data[hdr+7] ) return SQLITE_CORRUPT_PAGE(pPage);
    data[hdr+7] -= nFrag;
  }
  pTmp = &data[hdr+5];
  x = get2byte(pTmp);
  if( iStart<=x ){
    /* The new freeblock is at the beginning of the cell content area,
    ** so just extend the cell content area rather than create another
    ** freelist entry */
    if( iStart<x ) return SQLITE_CORRUPT_PAGE(pPage);
    if( iPtr!=hdr+1 ) return SQLITE_CORRUPT_PAGE(pPage);
    put2byte(&data[hdr+1], iFreeBlk);
    put2byte(&data[hdr+5], iEnd);
  }else{
    /* Insert the new freeblock into the freelist */
    put2byte(&data[iPtr], iStart);
  }
  if( pPage->pBt->btsFlags & BTS_FAST_SECURE ){
    /* Overwrite deleted information with zeros when the secure_delete
    ** option is enabled */
    memset(&data[iStart], 0, iSize);
  }
  put2byte(&data[iStart], iFreeBlk);
  put2byte(&data[iStart+2], iSize);
  pPage->nFree += iOrigSize;
  return SQLITE_OK;
}

/*
** Decode the flags byte (the first byte of the header) for a page
** and initialize fields of the MemPage structure accordingly.
**
** Only the following combinations are supported.  Anything different
** indicates a corrupt database files:
**
**         PTF_ZERODATA                             (0x02,  2)
**         PTF_LEAFDATA | PTF_INTKEY                (0x05,  5)
**         PTF_ZERODATA | PTF_LEAF                  (0x0a, 10)
**         PTF_LEAFDATA | PTF_INTKEY | PTF_LEAF     (0x0d, 13)
*/
static int decodeFlags(MemPage *pPage, int flagByte){
  BtShared *pBt;     /* A copy of pPage->pBt */

  assert( pPage->hdrOffset==(pPage->pgno==1 ? 100 : 0) );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  pBt = pPage->pBt;
  pPage->max1bytePayload = pBt->max1bytePayload;
  if( flagByte>=(PTF_ZERODATA | PTF_LEAF) ){
    pPage->childPtrSize = 0;
    pPage->leaf = 1;
    if( flagByte==(PTF_LEAFDATA | PTF_INTKEY | PTF_LEAF) ){
      pPage->intKeyLeaf = 1;
      pPage->xCellSize = cellSizePtrTableLeaf;
      pPage->xParseCell = btreeParseCellPtr;
      pPage->intKey = 1;
      pPage->maxLocal = pBt->maxLeaf;
      pPage->minLocal = pBt->minLeaf;
    }else if( flagByte==(PTF_ZERODATA | PTF_LEAF) ){
      pPage->intKey = 0;
      pPage->intKeyLeaf = 0;
      pPage->xCellSize = cellSizePtr;
      pPage->xParseCell = btreeParseCellPtrIndex;
      pPage->maxLocal = pBt->maxLocal;
      pPage->minLocal = pBt->minLocal;
    }else{
      pPage->intKey = 0;
      pPage->intKeyLeaf = 0;
      pPage->xCellSize = cellSizePtr;
      pPage->xParseCell = btreeParseCellPtrIndex;
      return SQLITE_CORRUPT_PAGE(pPage);
    }
  }else{
    pPage->childPtrSize = 4;
    pPage->leaf = 0;
    if( flagByte==(PTF_ZERODATA) ){
      pPage->intKey = 0;
      pPage->intKeyLeaf = 0;
      pPage->xCellSize = cellSizePtr;
      pPage->xParseCell = btreeParseCellPtrIndex;
      pPage->maxLocal = pBt->maxLocal;
      pPage->minLocal = pBt->minLocal;
    }else if( flagByte==(PTF_LEAFDATA | PTF_INTKEY) ){
      pPage->intKeyLeaf = 0;
      pPage->xCellSize = cellSizePtrNoPayload;
      pPage->xParseCell = btreeParseCellPtrNoPayload;
      pPage->intKey = 1;
      pPage->maxLocal = pBt->maxLeaf;
      pPage->minLocal = pBt->minLeaf;
    }else{
      pPage->intKey = 0;
      pPage->intKeyLeaf = 0;
      pPage->xCellSize = cellSizePtr;
      pPage->xParseCell = btreeParseCellPtrIndex;
      return SQLITE_CORRUPT_PAGE(pPage);
    }
  }
  return SQLITE_OK;
}

/*
** Compute the amount of freespace on the page.  In other words, fill
** in the pPage->nFree field.
*/
static int btreeComputeFreeSpace(MemPage *pPage){
  int pc;            /* Address of a freeblock within pPage->aData[] */
  u8 hdr;            /* Offset to beginning of page header */
  u8 *data;          /* Equal to pPage->aData */
  int usableSize;    /* Amount of usable space on each page */
  int nFree;         /* Number of unused bytes on the page */
  int top;           /* First byte of the cell content area */
  int iCellFirst;    /* First allowable cell or freeblock offset */
  int iCellLast;     /* Last possible cell or freeblock offset */

  assert( pPage->pBt!=0 );
  assert( pPage->pBt->db!=0 );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( pPage->pgno==sqlite3PagerPagenumber(pPage->pDbPage) );
  assert( pPage == sqlite3PagerGetExtra(pPage->pDbPage) );
  assert( pPage->aData == sqlite3PagerGetData(pPage->pDbPage) );
  assert( pPage->isInit==1 );
  assert( pPage->nFree<0 );

  usableSize = pPage->pBt->usableSize;
  hdr = pPage->hdrOffset;
  data = pPage->aData;
  /* EVIDENCE-OF: R-58015-48175 The two-byte integer at offset 5 designates
  ** the start of the cell content area. A zero value for this integer is
  ** interpreted as 65536. */
  top = get2byteNotZero(&data[hdr+5]);
  iCellFirst = hdr + 8 + pPage->childPtrSize + 2*pPage->nCell;
  iCellLast = usableSize - 4;

  /* Compute the total free space on the page
  ** EVIDENCE-OF: R-23588-34450 The two-byte integer at offset 1 gives the
  ** start of the first freeblock on the page, or is zero if there are no
  ** freeblocks. */
  pc = get2byte(&data[hdr+1]);
  nFree = data[hdr+7] + top;  /* Init nFree to non-freeblock free space */
  if( pc>0 ){
    u32 next, size;
    if( pc<top ){
      /* EVIDENCE-OF: R-55530-52930 In a well-formed b-tree page, there will
      ** always be at least one cell before the first freeblock.
      */
      return SQLITE_CORRUPT_PAGE(pPage); 
    }
    while( 1 ){
      if( pc>iCellLast ){
        /* Freeblock off the end of the page */
        return SQLITE_CORRUPT_PAGE(pPage);
      }
      next = get2byte(&data[pc]);
      size = get2byte(&data[pc+2]);
      nFree = nFree + size;
      if( next<=pc+size+3 ) break;
      pc = next;
    }
    if( next>0 ){
      /* Freeblock not in ascending order */
      return SQLITE_CORRUPT_PAGE(pPage);
    }
    if( pc+size>(unsigned int)usableSize ){
      /* Last freeblock extends past page end */
      return SQLITE_CORRUPT_PAGE(pPage);
    }
  }

  /* At this point, nFree contains the sum of the offset to the start
  ** of the cell-content area plus the number of free bytes within
  ** the cell-content area. If this is greater than the usable-size
  ** of the page, then the page must be corrupted. This check also
  ** serves to verify that the offset to the start of the cell-content
  ** area, according to the page header, lies within the page.
  */
  if( nFree>usableSize || nFree<iCellFirst ){
    return SQLITE_CORRUPT_PAGE(pPage);
  }
  pPage->nFree = (u16)(nFree - iCellFirst);
  return SQLITE_OK;
}

/*
** Do additional sanity check after btreeInitPage() if
** PRAGMA cell_size_check=ON 
*/
static SQLITE_NOINLINE int btreeCellSizeCheck(MemPage *pPage){
  int iCellFirst;    /* First allowable cell or freeblock offset */
  int iCellLast;     /* Last possible cell or freeblock offset */
  int i;             /* Index into the cell pointer array */
  int sz;            /* Size of a cell */
  int pc;            /* Address of a freeblock within pPage->aData[] */
  u8 *data;          /* Equal to pPage->aData */
  int usableSize;    /* Maximum usable space on the page */
  int cellOffset;    /* Start of cell content area */

  iCellFirst = pPage->cellOffset + 2*pPage->nCell;
  usableSize = pPage->pBt->usableSize;
  iCellLast = usableSize - 4;
  data = pPage->aData;
  cellOffset = pPage->cellOffset;
  if( !pPage->leaf ) iCellLast--;
  for(i=0; i<pPage->nCell; i++){
    pc = get2byteAligned(&data[cellOffset+i*2]);
    testcase( pc==iCellFirst );
    testcase( pc==iCellLast );
    if( pc<iCellFirst || pc>iCellLast ){
      return SQLITE_CORRUPT_PAGE(pPage);
    }
    sz = pPage->xCellSize(pPage, &data[pc]);
    testcase( pc+sz==usableSize );
    if( pc+sz>usableSize ){
      return SQLITE_CORRUPT_PAGE(pPage);
    }
  }
  return SQLITE_OK;
}

/*
** Initialize the auxiliary information for a disk block.
**
** Return SQLITE_OK on success.  If we see that the page does
** not contain a well-formed database page, then return 
** SQLITE_CORRUPT.  Note that a return of SQLITE_OK does not
** guarantee that the page is well-formed.  It only shows that
** we failed to detect any corruption.
*/
static int btreeInitPage(MemPage *pPage){
  u8 *data;          /* Equal to pPage->aData */
  BtShared *pBt;        /* The main btree structure */

  assert( pPage->pBt!=0 );
  assert( pPage->pBt->db!=0 );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( pPage->pgno==sqlite3PagerPagenumber(pPage->pDbPage) );
  assert( pPage == sqlite3PagerGetExtra(pPage->pDbPage) );
  assert( pPage->aData == sqlite3PagerGetData(pPage->pDbPage) );
  assert( pPage->isInit==0 );

  pBt = pPage->pBt;
  data = pPage->aData + pPage->hdrOffset;
  /* EVIDENCE-OF: R-28594-02890 The one-byte flag at offset 0 indicating
  ** the b-tree page type. */
  if( decodeFlags(pPage, data[0]) ){
    return SQLITE_CORRUPT_PAGE(pPage);
  }
  assert( pBt->pageSize>=512 && pBt->pageSize<=65536 );
  pPage->maskPage = (u16)(pBt->pageSize - 1);
  pPage->nOverflow = 0;
  pPage->cellOffset = pPage->hdrOffset + 8 + pPage->childPtrSize;
  pPage->aCellIdx = data + pPage->childPtrSize + 8;
  pPage->aDataEnd = pPage->aData + pBt->pageSize;
  pPage->aDataOfst = pPage->aData + pPage->childPtrSize;
  /* EVIDENCE-OF: R-37002-32774 The two-byte integer at offset 3 gives the
  ** number of cells on the page. */
  pPage->nCell = get2byte(&data[3]);
  if( pPage->nCell>MX_CELL(pBt) ){
    /* To many cells for a single page.  The page must be corrupt */
    return SQLITE_CORRUPT_PAGE(pPage);
  }
  testcase( pPage->nCell==MX_CELL(pBt) );
  /* EVIDENCE-OF: R-24089-57979 If a page contains no cells (which is only
  ** possible for a root page of a table that contains no rows) then the
  ** offset to the cell content area will equal the page size minus the
  ** bytes of reserved space. */
  assert( pPage->nCell>0
       || get2byteNotZero(&data[5])==(int)pBt->usableSize
       || CORRUPT_DB );
  pPage->nFree = -1;  /* Indicate that this value is yet uncomputed */
  pPage->isInit = 1;
  if( pBt->db->flags & SQLITE_CellSizeCk ){
    return btreeCellSizeCheck(pPage);
  }
  return SQLITE_OK;
}

/*
** 将原始页面初始化为空数据库页面的标准结构。
*/
static void zeroPage(MemPage *pPage, int flags){
  unsigned char *data = pPage->aData;
  BtShared *pBt = pPage->pBt;
  u8 hdr = pPage->hdrOffset;
  u16 first;

  assert( sqlite3PagerPagenumber(pPage->pDbPage)==pPage->pgno || CORRUPT_DB );
  assert( sqlite3PagerGetExtra(pPage->pDbPage) == (void*)pPage );
  assert( sqlite3PagerGetData(pPage->pDbPage) == data );
  assert( sqlite3PagerIswriteable(pPage->pDbPage) );
  assert( sqlite3_mutex_held(pBt->mutex) );
  if( pBt->btsFlags & BTS_FAST_SECURE ){
    memset(&data[hdr], 0, pBt->usableSize - hdr);
  }
  data[hdr] = (char)flags;
  first = hdr + ((flags&PTF_LEAF)==0 ? 12 : 8);
  memset(&data[hdr+1], 0, 4);
  data[hdr+7] = 0;
  put2byte(&data[hdr+5], pBt->usableSize);
  pPage->nFree = (u16)(pBt->usableSize - first);
  decodeFlags(pPage, flags);
  pPage->cellOffset = first;
  pPage->aDataEnd = &data[pBt->pageSize];
  pPage->aCellIdx = &data[first];
  pPage->aDataOfst = &data[pPage->childPtrSize];
  pPage->nOverflow = 0;
  assert( pBt->pageSize>=512 && pBt->pageSize<=65536 );
  pPage->maskPage = (u16)(pBt->pageSize - 1);
  pPage->nCell = 0;
  pPage->isInit = 1;
}


/*
** Convert a DbPage obtained from the pager into a MemPage used by
** the btree layer.
*/
static MemPage *btreePageFromDbPage(DbPage *pDbPage, Pgno pgno, BtShared *pBt){
  MemPage *pPage = (MemPage*)sqlite3PagerGetExtra(pDbPage);
  if( pgno!=pPage->pgno ){
    pPage->aData = sqlite3PagerGetData(pDbPage);
    pPage->pDbPage = pDbPage;
    pPage->pBt = pBt;
    pPage->pgno = pgno;
    pPage->hdrOffset = pgno==1 ? 100 : 0;
  }
  assert( pPage->aData==sqlite3PagerGetData(pDbPage) );
  return pPage; 
}

/*
** Get a page from the pager.  Initialize the MemPage.pBt and
** MemPage.aData elements if needed.  See also: btreeGetUnusedPage().
**
** If the PAGER_GET_NOCONTENT flag is set, it means that we do not care
** about the content of the page at this time.  So do not go to the disk
** to fetch the content.  Just fill in the content with zeros for now.
** If in the future we call sqlite3PagerWrite() on this page, that
** means we have started to be concerned about content and the disk
** read should occur at that point.
*/
static int btreeGetPage(
  BtShared *pBt,       /* The btree */
  Pgno pgno,           /* Number of the page to fetch */
  MemPage **ppPage,    /* Return the page in this parameter */
  int flags            /* PAGER_GET_NOCONTENT or PAGER_GET_READONLY */
){
  int rc;
  DbPage *pDbPage;

  assert( flags==0 || flags==PAGER_GET_NOCONTENT || flags==PAGER_GET_READONLY );
  assert( sqlite3_mutex_held(pBt->mutex) );
  rc = sqlite3PagerGet(pBt->pPager, pgno, (DbPage**)&pDbPage, flags);
  if( rc ) return rc;
  *ppPage = btreePageFromDbPage(pDbPage, pgno, pBt);
  return SQLITE_OK;
}

/*
** Retrieve a page from the pager cache. If the requested page is not
** already in the pager cache return NULL. Initialize the MemPage.pBt and
** MemPage.aData elements if needed.
*/
static MemPage *btreePageLookup(BtShared *pBt, Pgno pgno){
  DbPage *pDbPage;
  assert( sqlite3_mutex_held(pBt->mutex) );
  pDbPage = sqlite3PagerLookup(pBt->pPager, pgno);
  if( pDbPage ){
    return btreePageFromDbPage(pDbPage, pgno, pBt);
  }
  return 0;
}

/*
** Return the size of the database file in pages. If there is any kind of
** error, return ((unsigned int)-1).
*/
static Pgno btreePagecount(BtShared *pBt){
  return pBt->nPage;
}
Pgno sqlite3BtreeLastPage(Btree *p){
  assert( sqlite3BtreeHoldsMutex(p) );
  return btreePagecount(p->pBt);
}

/*
** Get a page from the pager and initialize it.
**
** If pCur!=0 then the page is being fetched as part of a moveToChild()
** call.  Do additional sanity checking on the page in this case.
** And if the fetch fails, this routine must decrement pCur->iPage.
**
** The page is fetched as read-write unless pCur is not NULL and is
** a read-only cursor.
**
** If an error occurs, then *ppPage is undefined. It
** may remain unchanged, or it may be set to an invalid value.
*/
static int getAndInitPage(
  BtShared *pBt,                  /* The database file */
  Pgno pgno,                      /* Number of the page to get */
  MemPage **ppPage,               /* Write the page pointer here */
  BtCursor *pCur,                 /* Cursor to receive the page, or NULL */
  int bReadOnly                   /* True for a read-only page */
){
  int rc;
  DbPage *pDbPage;
  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( pCur==0 || ppPage==&pCur->pPage );
  assert( pCur==0 || bReadOnly==pCur->curPagerFlags );
  assert( pCur==0 || pCur->iPage>0 );

  if( pgno>btreePagecount(pBt) ){
    rc = SQLITE_CORRUPT_BKPT;
    goto getAndInitPage_error1;
  }
  rc = sqlite3PagerGet(pBt->pPager, pgno, (DbPage**)&pDbPage, bReadOnly);
  if( rc ){
    goto getAndInitPage_error1;
  }
  *ppPage = (MemPage*)sqlite3PagerGetExtra(pDbPage);
  if( (*ppPage)->isInit==0 ){
    btreePageFromDbPage(pDbPage, pgno, pBt);
    rc = btreeInitPage(*ppPage);
    if( rc!=SQLITE_OK ){
      goto getAndInitPage_error2;
    }
  }
  assert( (*ppPage)->pgno==pgno || CORRUPT_DB );
  assert( (*ppPage)->aData==sqlite3PagerGetData(pDbPage) );

  /* If obtaining a child page for a cursor, we must verify that the page is
  ** compatible with the root page. */
  if( pCur && ((*ppPage)->nCell<1 || (*ppPage)->intKey!=pCur->curIntKey) ){
    rc = SQLITE_CORRUPT_PGNO(pgno);
    goto getAndInitPage_error2;
  }
  return SQLITE_OK;

getAndInitPage_error2:
  releasePage(*ppPage);
getAndInitPage_error1:
  if( pCur ){
    pCur->iPage--;
    pCur->pPage = pCur->apPage[pCur->iPage];
  }
  testcase( pgno==0 );
  assert( pgno!=0 || rc!=SQLITE_OK );
  return rc;
}

/*
** Release a MemPage.  This should be called once for each prior
** call to btreeGetPage.
**
** Page1 is a special case and must be released using releasePageOne().
*/
static void releasePageNotNull(MemPage *pPage){
  assert( pPage->aData );
  assert( pPage->pBt );
  assert( pPage->pDbPage!=0 );
  assert( sqlite3PagerGetExtra(pPage->pDbPage) == (void*)pPage );
  assert( sqlite3PagerGetData(pPage->pDbPage)==pPage->aData );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  sqlite3PagerUnrefNotNull(pPage->pDbPage);
}
static void releasePage(MemPage *pPage){
  if( pPage ) releasePageNotNull(pPage);
}
static void releasePageOne(MemPage *pPage){
  assert( pPage!=0 );
  assert( pPage->aData );
  assert( pPage->pBt );
  assert( pPage->pDbPage!=0 );
  assert( sqlite3PagerGetExtra(pPage->pDbPage) == (void*)pPage );
  assert( sqlite3PagerGetData(pPage->pDbPage)==pPage->aData );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  sqlite3PagerUnrefPageOne(pPage->pDbPage);
}

/*
** Get an unused page.
**
** This works just like btreeGetPage() with the addition:
**
**   *  If the page is already in use for some other purpose, immediately
**      release it and return an SQLITE_CURRUPT error.
**   *  Make sure the isInit flag is clear
*/
static int btreeGetUnusedPage(
  BtShared *pBt,       /* The btree */
  Pgno pgno,           /* Number of the page to fetch */
  MemPage **ppPage,    /* Return the page in this parameter */
  int flags            /* PAGER_GET_NOCONTENT or PAGER_GET_READONLY */
){
  int rc = btreeGetPage(pBt, pgno, ppPage, flags);
  if( rc==SQLITE_OK ){
    if( sqlite3PagerPageRefcount((*ppPage)->pDbPage)>1 ){
      releasePage(*ppPage);
      *ppPage = 0;
      return SQLITE_CORRUPT_BKPT;
    }
    (*ppPage)->isInit = 0;
  }else{
    *ppPage = 0;
  }
  return rc;
}


/*
** During a rollback, when the pager reloads information into the cache
** so that the cache is restored to its original state at the start of
** the transaction, for each page restored this routine is called.
**
** This routine needs to reset the extra data section at the end of the
** page to agree with the restored data.
*/
static void pageReinit(DbPage *pData){
  MemPage *pPage;
  pPage = (MemPage *)sqlite3PagerGetExtra(pData);
  assert( sqlite3PagerPageRefcount(pData)>0 );
  if( pPage->isInit ){
    assert( sqlite3_mutex_held(pPage->pBt->mutex) );
    pPage->isInit = 0;
    if( sqlite3PagerPageRefcount(pData)>1 ){
      /* pPage might not be a btree page;  it might be an overflow page
      ** or ptrmap page or a free page.  In those cases, the following
      ** call to btreeInitPage() will likely return SQLITE_CORRUPT.
      ** But no harm is done by this.  And it is very important that
      ** btreeInitPage() be called on every btree page so we make
      ** the call for every page that comes in for re-initing. */
      btreeInitPage(pPage);
    }
  }
}

/*
** Invoke the busy handler for a btree.
*/
static int btreeInvokeBusyHandler(void *pArg){
  BtShared *pBt = (BtShared*)pArg;
  assert( pBt->db );
  assert( sqlite3_mutex_held(pBt->db->mutex) );
  return sqlite3InvokeBusyHandler(&pBt->db->busyHandler);
}

/*
zFilename 是数据库文件的名称。
若 zFilename 为 NULL，则会创建一个临时数据库。此临时数据库可能完全存在于内存中，或使用基于磁盘的内存缓存。无论是哪种情况，临时数据库在调用sqlite3BtreeClose() 时会被自动删除。
若 zFilename 为 ":memory:"，则创建一个在内存中的数据库，该数据库在关闭时自动销毁。

flags 参数是一个位掩码，可能包含如下标志位：BTREE_OMIT_JOURNAL（省略日志）、BTREE_MEMORY（内存数据库）。

如果数据库已在同一数据库连接中打开，且当前处于共享缓存模式（shared cache mode），则此次打开操作将失败并返回 SQLITE_CONSTRAINT 错误。
在同一数据库连接中，我们不允许存在多个 BtShared 对象，否则会导致锁定（locking）问题。
*/
int sqlite3BtreeOpen(
  sqlite3_vfs *pVfs,      /* VFS to use for this b-tree */
  const char *zFilename,  /* Name of the file containing the BTree database */
  sqlite3 *db,            /* Associated database handle */
  Btree **ppBtree,        /* Pointer to new Btree object written here */
  int flags,              /* Options */
  int vfsFlags            /* Flags passed through to sqlite3_vfs.xOpen() */
){
  BtShared *pBt = 0;             /* Shared part of btree structure */
  Btree *p;                      /* Handle to return */
  sqlite3_mutex *mutexOpen = 0;  /* Prevents a race condition. Ticket #3537 */
  int rc = SQLITE_OK;            /* Result code from this function */
  u8 nReserve;                   /* Byte of unused space on each page */
  unsigned char zDbHeader[100];  /* Database header content */

  /* True if opening an ephemeral, temporary database */
  const int isTempDb = zFilename==0 || zFilename[0]==0;

  /* Set the variable isMemdb to true for an in-memory database, or 
  ** false for a file-based database.
  */
#ifdef SQLITE_OMIT_MEMORYDB
  const int isMemdb = 0;
#else
  const int isMemdb = (zFilename && strcmp(zFilename, ":memory:")==0)
                       || (isTempDb && sqlite3TempInMemory(db))
                       || (vfsFlags & SQLITE_OPEN_MEMORY)!=0;
#endif

  assert( db!=0 );
  assert( pVfs!=0 );
  assert( sqlite3_mutex_held(db->mutex) );
  assert( (flags&0xff)==flags );   /* flags fit in 8 bits */

  /* Only a BTREE_SINGLE database can be BTREE_UNORDERED */
  assert( (flags & BTREE_UNORDERED)==0 || (flags & BTREE_SINGLE)!=0 );

  /* A BTREE_SINGLE database is always a temporary and/or ephemeral */
  assert( (flags & BTREE_SINGLE)==0 || isTempDb );

  if( isMemdb ){
    flags |= BTREE_MEMORY;
  }
  if( (vfsFlags & SQLITE_OPEN_MAIN_DB)!=0 && (isMemdb || isTempDb) ){
    vfsFlags = (vfsFlags & ~SQLITE_OPEN_MAIN_DB) | SQLITE_OPEN_TEMP_DB;
  }
  p = sqlite3MallocZero(sizeof(Btree));
  if( !p ){
    return SQLITE_NOMEM_BKPT;
  }
  p->inTrans = TRANS_NONE;
  p->db = db;
#ifndef SQLITE_OMIT_SHARED_CACHE
  p->lock.pBtree = p;
  p->lock.iTable = 1;
#endif

#if !defined(SQLITE_OMIT_SHARED_CACHE) && !defined(SQLITE_OMIT_DISKIO)
  /*
  ** If this Btree is a candidate for shared cache, try to find an
  ** existing BtShared object that we can share with
  */
  if( isTempDb==0 && (isMemdb==0 || (vfsFlags&SQLITE_OPEN_URI)!=0) ){
    if( vfsFlags & SQLITE_OPEN_SHAREDCACHE ){
      int nFilename = sqlite3Strlen30(zFilename)+1;
      int nFullPathname = pVfs->mxPathname+1;
      char *zFullPathname = sqlite3Malloc(MAX(nFullPathname,nFilename));
      MUTEX_LOGIC( sqlite3_mutex *mutexShared; )

      p->sharable = 1;
      if( !zFullPathname ){
        sqlite3_free(p);
        return SQLITE_NOMEM_BKPT;
      }
      if( isMemdb ){
        memcpy(zFullPathname, zFilename, nFilename);
      }else{
        rc = sqlite3OsFullPathname(pVfs, zFilename,
                                   nFullPathname, zFullPathname);
        if( rc ){
          if( rc==SQLITE_OK_SYMLINK ){
            rc = SQLITE_OK;
          }else{
            sqlite3_free(zFullPathname);
            sqlite3_free(p);
            return rc;
          }
        }
      }
#if SQLITE_THREADSAFE
      mutexOpen = sqlite3MutexAlloc(SQLITE_MUTEX_STATIC_OPEN);
      sqlite3_mutex_enter(mutexOpen);
      mutexShared = sqlite3MutexAlloc(SQLITE_MUTEX_STATIC_MAIN);
      sqlite3_mutex_enter(mutexShared);
#endif
      for(pBt=GLOBAL(BtShared*,sqlite3SharedCacheList); pBt; pBt=pBt->pNext){
        assert( pBt->nRef>0 );
        if( 0==strcmp(zFullPathname, sqlite3PagerFilename(pBt->pPager, 0))
                 && sqlite3PagerVfs(pBt->pPager)==pVfs ){
          int iDb;
          for(iDb=db->nDb-1; iDb>=0; iDb--){
            Btree *pExisting = db->aDb[iDb].pBt;
            if( pExisting && pExisting->pBt==pBt ){
              sqlite3_mutex_leave(mutexShared);
              sqlite3_mutex_leave(mutexOpen);
              sqlite3_free(zFullPathname);
              sqlite3_free(p);
              return SQLITE_CONSTRAINT;
            }
          }
          p->pBt = pBt;
          pBt->nRef++;
          break;
        }
      }
      sqlite3_mutex_leave(mutexShared);
      sqlite3_free(zFullPathname);
    }
#ifdef SQLITE_DEBUG
    else{
      /* In debug mode, we mark all persistent databases as sharable
      ** even when they are not.  This exercises the locking code and
      ** gives more opportunity for asserts(sqlite3_mutex_held())
      ** statements to find locking problems.
      */
      p->sharable = 1;
    }
#endif
  }
#endif
  if( pBt==0 ){
    /*
    ** The following asserts make sure that structures used by the btree are
    ** the right size.  This is to guard against size changes that result
    ** when compiling on a different architecture.
    */
    assert( sizeof(i64)==8 );
    assert( sizeof(u64)==8 );
    assert( sizeof(u32)==4 );
    assert( sizeof(u16)==2 );
    assert( sizeof(Pgno)==4 );
  
    pBt = sqlite3MallocZero( sizeof(*pBt) );
    if( pBt==0 ){
      rc = SQLITE_NOMEM_BKPT;
      goto btree_open_out;
    }
    rc = sqlite3PagerOpen(pVfs, &pBt->pPager, zFilename,
                          sizeof(MemPage), flags, vfsFlags, pageReinit);
    if( rc==SQLITE_OK ){
      sqlite3PagerSetMmapLimit(pBt->pPager, db->szMmap);
      rc = sqlite3PagerReadFileheader(pBt->pPager,sizeof(zDbHeader),zDbHeader);
    }
    if( rc!=SQLITE_OK ){
      goto btree_open_out;
    }
    pBt->openFlags = (u8)flags;
    pBt->db = db;
    sqlite3PagerSetBusyHandler(pBt->pPager, btreeInvokeBusyHandler, pBt);
    p->pBt = pBt;
  
    pBt->pCursor = 0;
    pBt->pPage1 = 0;
    if( sqlite3PagerIsreadonly(pBt->pPager) ) pBt->btsFlags |= BTS_READ_ONLY;
#if defined(SQLITE_SECURE_DELETE)
    pBt->btsFlags |= BTS_SECURE_DELETE;
#elif defined(SQLITE_FAST_SECURE_DELETE)
    pBt->btsFlags |= BTS_OVERWRITE;
#endif
    /* EVIDENCE-OF: R-51873-39618 The page size for a database file is
    ** determined by the 2-byte integer located at an offset of 16 bytes from
    ** the beginning of the database file. */
    pBt->pageSize = (zDbHeader[16]<<8) | (zDbHeader[17]<<16);
    if( pBt->pageSize<512 || pBt->pageSize>SQLITE_MAX_PAGE_SIZE
         || ((pBt->pageSize-1)&pBt->pageSize)!=0 ){
      pBt->pageSize = 0;
#ifndef SQLITE_OMIT_AUTOVACUUM
      /* If the magic name ":memory:" will create an in-memory database, then
      ** leave the autoVacuum mode at 0 (do not auto-vacuum), even if
      ** SQLITE_DEFAULT_AUTOVACUUM is true. On the other hand, if
      ** SQLITE_OMIT_MEMORYDB has been defined, then ":memory:" is just a
      ** regular file-name. In this case the auto-vacuum applies as per normal.
      */
      if( zFilename && !isMemdb ){
        pBt->autoVacuum = (SQLITE_DEFAULT_AUTOVACUUM ? 1 : 0);
        pBt->incrVacuum = (SQLITE_DEFAULT_AUTOVACUUM==2 ? 1 : 0);
      }
#endif
      nReserve = 0;
    }else{
      /* EVIDENCE-OF: R-37497-42412 The size of the reserved region is
      ** determined by the one-byte unsigned integer found at an offset of 20
      ** into the database file header. */
      nReserve = zDbHeader[20];
      pBt->btsFlags |= BTS_PAGESIZE_FIXED;
#ifndef SQLITE_OMIT_AUTOVACUUM
      pBt->autoVacuum = (get4byte(&zDbHeader[36 + 4*4])?1:0);
      pBt->incrVacuum = (get4byte(&zDbHeader[36 + 7*4])?1:0);
#endif
    }
    rc = sqlite3PagerSetPagesize(pBt->pPager, &pBt->pageSize, nReserve);
    if( rc ) goto btree_open_out;
    pBt->usableSize = pBt->pageSize - nReserve;
    assert( (pBt->pageSize & 7)==0 );  /* 8-byte alignment of pageSize */
   
#if !defined(SQLITE_OMIT_SHARED_CACHE) && !defined(SQLITE_OMIT_DISKIO)
    /* Add the new BtShared object to the linked list sharable BtShareds.
    */
    pBt->nRef = 1;
    if( p->sharable ){
      MUTEX_LOGIC( sqlite3_mutex *mutexShared; )
      MUTEX_LOGIC( mutexShared = sqlite3MutexAlloc(SQLITE_MUTEX_STATIC_MAIN);)
      if( SQLITE_THREADSAFE && sqlite3GlobalConfig.bCoreMutex ){
        pBt->mutex = sqlite3MutexAlloc(SQLITE_MUTEX_FAST);
        if( pBt->mutex==0 ){
          rc = SQLITE_NOMEM_BKPT;
          goto btree_open_out;
        }
      }
      sqlite3_mutex_enter(mutexShared);
      pBt->pNext = GLOBAL(BtShared*,sqlite3SharedCacheList);
      GLOBAL(BtShared*,sqlite3SharedCacheList) = pBt;
      sqlite3_mutex_leave(mutexShared);
    }
#endif
  }

#if !defined(SQLITE_OMIT_SHARED_CACHE) && !defined(SQLITE_OMIT_DISKIO)
  /* If the new Btree uses a sharable pBtShared, then link the new
  ** Btree into the list of all sharable Btrees for the same connection.
  ** The list is kept in ascending order by pBt address.
  */
  if( p->sharable ){
    int i;
    Btree *pSib;
    for(i=0; i<db->nDb; i++){
      if( (pSib = db->aDb[i].pBt)!=0 && pSib->sharable ){
        while( pSib->pPrev ){ pSib = pSib->pPrev; }
        if( (uptr)p->pBt<(uptr)pSib->pBt ){
          p->pNext = pSib;
          p->pPrev = 0;
          pSib->pPrev = p;
        }else{
          while( pSib->pNext && (uptr)pSib->pNext->pBt<(uptr)p->pBt ){
            pSib = pSib->pNext;
          }
          p->pNext = pSib->pNext;
          p->pPrev = pSib;
          if( p->pNext ){
            p->pNext->pPrev = p;
          }
          pSib->pNext = p;
        }
        break;
      }
    }
  }
#endif
  *ppBtree = p;

btree_open_out:
  if( rc!=SQLITE_OK ){
    if( pBt && pBt->pPager ){
      sqlite3PagerClose(pBt->pPager, 0);
    }
    sqlite3_free(pBt);
    sqlite3_free(p);
    *ppBtree = 0;
  }else{
    sqlite3_file *pFile;

    /* If the B-Tree was successfully opened, set the pager-cache size to the
    ** default value. Except, when opening on an existing shared pager-cache,
    ** do not change the pager-cache size.
    */
    if( sqlite3BtreeSchema(p, 0, 0)==0 ){
      sqlite3BtreeSetCacheSize(p, SQLITE_DEFAULT_CACHE_SIZE);
    }

    pFile = sqlite3PagerFile(pBt->pPager);
    if( pFile->pMethods ){
      sqlite3OsFileControlHint(pFile, SQLITE_FCNTL_PDB, (void*)&pBt->db);
    }
  }
  if( mutexOpen ){
    assert( sqlite3_mutex_held(mutexOpen) );
    sqlite3_mutex_leave(mutexOpen);
  }
  assert( rc!=SQLITE_OK || sqlite3BtreeConnectionCount(*ppBtree)>0 );
  return rc;
}

/*
** Decrement the BtShared.nRef counter.  When it reaches zero,
** remove the BtShared structure from the sharing list.  Return
** true if the BtShared.nRef counter reaches zero and return
** false if it is still positive.
*/
static int removeFromSharingList(BtShared *pBt){
#ifndef SQLITE_OMIT_SHARED_CACHE
  MUTEX_LOGIC( sqlite3_mutex *pMainMtx; )
  BtShared *pList;
  int removed = 0;

  assert( sqlite3_mutex_notheld(pBt->mutex) );
  MUTEX_LOGIC( pMainMtx = sqlite3MutexAlloc(SQLITE_MUTEX_STATIC_MAIN); )
  sqlite3_mutex_enter(pMainMtx);
  pBt->nRef--;
  if( pBt->nRef<=0 ){
    if( GLOBAL(BtShared*,sqlite3SharedCacheList)==pBt ){
      GLOBAL(BtShared*,sqlite3SharedCacheList) = pBt->pNext;
    }else{
      pList = GLOBAL(BtShared*,sqlite3SharedCacheList);
      while( ALWAYS(pList) && pList->pNext!=pBt ){
        pList=pList->pNext;
      }
      if( ALWAYS(pList) ){
        pList->pNext = pBt->pNext;
      }
    }
    if( SQLITE_THREADSAFE ){
      sqlite3_mutex_free(pBt->mutex);
    }
    removed = 1;
  }
  sqlite3_mutex_leave(pMainMtx);
  return removed;
#else
  return 1;
#endif
}

/*
** Make sure pBt->pTmpSpace points to an allocation of 
** MX_CELL_SIZE(pBt) bytes with a 4-byte prefix for a left-child
** pointer.
*/
static SQLITE_NOINLINE int allocateTempSpace(BtShared *pBt){
  assert( pBt!=0 );
  assert( pBt->pTmpSpace==0 );
  /* This routine is called only by btreeCursor() when allocating the
  ** first write cursor for the BtShared object */
  assert( pBt->pCursor!=0 && (pBt->pCursor->curFlags & BTCF_WriteFlag)!=0 );
  pBt->pTmpSpace = sqlite3PageMalloc( pBt->pageSize );
  if( pBt->pTmpSpace==0 ){
    BtCursor *pCur = pBt->pCursor;
    pBt->pCursor = pCur->pNext;  /* Unlink the cursor */
    memset(pCur, 0, sizeof(*pCur));
    return SQLITE_NOMEM_BKPT;
  }

  /* One of the uses of pBt->pTmpSpace is to format cells before
  ** inserting them into a leaf page (function fillInCell()). If
  ** a cell is less than 4 bytes in size, it is rounded up to 4 bytes
  ** by the various routines that manipulate binary cells. Which
  ** can mean that fillInCell() only initializes the first 2 or 3
  ** bytes of pTmpSpace, but that the first 4 bytes are copied from
  ** it into a database page. This is not actually a problem, but it
  ** does cause a valgrind error when the 1 or 2 bytes of unitialized 
  ** data is passed to system call write(). So to avoid this error,
  ** zero the first 4 bytes of temp space here.
  **
  ** Also:  Provide four bytes of initialized space before the
  ** beginning of pTmpSpace as an area available to prepend the
  ** left-child pointer to the beginning of a cell.
  */
  memset(pBt->pTmpSpace, 0, 8);
  pBt->pTmpSpace += 4;
  return SQLITE_OK;
}

/*
** Free the pBt->pTmpSpace allocation
*/
static void freeTempSpace(BtShared *pBt){
  if( pBt->pTmpSpace ){
    pBt->pTmpSpace -= 4;
    sqlite3PageFree(pBt->pTmpSpace);
    pBt->pTmpSpace = 0;
  }
}

/*
** Close an open database and invalidate all cursors.
*/
int sqlite3BtreeClose(Btree *p){
  BtShared *pBt = p->pBt;

  /* Close all cursors opened via this handle.  */
  assert( sqlite3_mutex_held(p->db->mutex) );
  sqlite3BtreeEnter(p);

  /* Verify that no other cursors have this Btree open */
#ifdef SQLITE_DEBUG
  {
    BtCursor *pCur = pBt->pCursor;
    while( pCur ){
      BtCursor *pTmp = pCur;
      pCur = pCur->pNext;
      assert( pTmp->pBtree!=p );

    }
  }
#endif

  /* Rollback any active transaction and free the handle structure.
  ** The call to sqlite3BtreeRollback() drops any table-locks held by
  ** this handle.
  */
  sqlite3BtreeRollback(p, SQLITE_OK, 0);
  sqlite3BtreeLeave(p);

  /* If there are still other outstanding references to the shared-btree
  ** structure, return now. The remainder of this procedure cleans 
  ** up the shared-btree.
  */
  assert( p->wantToLock==0 && p->locked==0 );
  if( !p->sharable || removeFromSharingList(pBt) ){
    /* The pBt is no longer on the sharing list, so we can access
    ** it without having to hold the mutex.
    **
    ** Clean out and delete the BtShared object.
    */
    assert( !pBt->pCursor );
    sqlite3PagerClose(pBt->pPager, p->db);
    if( pBt->xFreeSchema && pBt->pSchema ){
      pBt->xFreeSchema(pBt->pSchema);
    }
    sqlite3DbFree(0, pBt->pSchema);
    freeTempSpace(pBt);
    sqlite3_free(pBt);
  }

#ifndef SQLITE_OMIT_SHARED_CACHE
  assert( p->wantToLock==0 );
  assert( p->locked==0 );
  if( p->pPrev ) p->pPrev->pNext = p->pNext;
  if( p->pNext ) p->pNext->pPrev = p->pPrev;
#endif

  sqlite3_free(p);
  return SQLITE_OK;
}

/*
** Change the "soft" limit on the number of pages in the cache.
** Unused and unmodified pages will be recycled when the number of
** pages in the cache exceeds this soft limit.  But the size of the
** cache is allowed to grow larger than this limit if it contains
** dirty pages or pages still in active use.
*/
int sqlite3BtreeSetCacheSize(Btree *p, int mxPage){
  BtShared *pBt = p->pBt;
  assert( sqlite3_mutex_held(p->db->mutex) );
  sqlite3BtreeEnter(p);
  sqlite3PagerSetCachesize(pBt->pPager, mxPage);
  sqlite3BtreeLeave(p);
  return SQLITE_OK;
}

/*
** Change the "spill" limit on the number of pages in the cache.
** If the number of pages exceeds this limit during a write transaction,
** the pager might attempt to "spill" pages to the journal early in
** order to free up memory.
**
** The value returned is the current spill size.  If zero is passed
** as an argument, no changes are made to the spill size setting, so
** using mxPage of 0 is a way to query the current spill size.
*/
int sqlite3BtreeSetSpillSize(Btree *p, int mxPage){
  BtShared *pBt = p->pBt;
  int res;
  assert( sqlite3_mutex_held(p->db->mutex) );
  sqlite3BtreeEnter(p);
  res = sqlite3PagerSetSpillsize(pBt->pPager, mxPage);
  sqlite3BtreeLeave(p);
  return res;
}

#if SQLITE_MAX_MMAP_SIZE>0
/*
** Change the limit on the amount of the database file that may be
** memory mapped.
*/
int sqlite3BtreeSetMmapLimit(Btree *p, sqlite3_int64 szMmap){
  BtShared *pBt = p->pBt;
  assert( sqlite3_mutex_held(p->db->mutex) );
  sqlite3BtreeEnter(p);
  sqlite3PagerSetMmapLimit(pBt->pPager, szMmap);
  sqlite3BtreeLeave(p);
  return SQLITE_OK;
}
#endif /* SQLITE_MAX_MMAP_SIZE>0 */

/*
** Change the way data is synced to disk in order to increase or decrease
** how well the database resists damage due to OS crashes and power
** failures.  Level 1 is the same as asynchronous (no syncs() occur and
** there is a high probability of damage)  Level 2 is the default.  There
** is a very low but non-zero probability of damage.  Level 3 reduces the
** probability of damage to near zero but with a write performance reduction.
*/
#ifndef SQLITE_OMIT_PAGER_PRAGMAS
int sqlite3BtreeSetPagerFlags(
  Btree *p,              /* The btree to set the safety level on */
  unsigned pgFlags       /* Various PAGER_* flags */
){
  BtShared *pBt = p->pBt;
  assert( sqlite3_mutex_held(p->db->mutex) );
  sqlite3BtreeEnter(p);
  sqlite3PagerSetFlags(pBt->pPager, pgFlags);
  sqlite3BtreeLeave(p);
  return SQLITE_OK;
}
#endif

/*
** Change the default pages size and the number of reserved bytes per page.
** Or, if the page size has already been fixed, return SQLITE_READONLY 
** without changing anything.
**
** The page size must be a power of 2 between 512 and 65536.  If the page
** size supplied does not meet this constraint then the page size is not
** changed.
**
** Page sizes are constrained to be a power of two so that the region
** of the database file used for locking (beginning at PENDING_BYTE,
** the first byte past the 1GB boundary, 0x40000000) needs to occur
** at the beginning of a page.
**
** If parameter nReserve is less than zero, then the number of reserved
** bytes per page is left unchanged.
**
** If the iFix!=0 then the BTS_PAGESIZE_FIXED flag is set so that the page size
** and autovacuum mode can no longer be changed.
*/
int sqlite3BtreeSetPageSize(Btree *p, int pageSize, int nReserve, int iFix){
  int rc = SQLITE_OK;
  int x;
  BtShared *pBt = p->pBt;
  assert( nReserve>=0 && nReserve<=255 );
  sqlite3BtreeEnter(p);
  pBt->nReserveWanted = nReserve;
  x = pBt->pageSize - pBt->usableSize;
  if( nReserve<x ) nReserve = x;
  if( pBt->btsFlags & BTS_PAGESIZE_FIXED ){
    sqlite3BtreeLeave(p);
    return SQLITE_READONLY;
  }
  assert( nReserve>=0 && nReserve<=255 );
  if( pageSize>=512 && pageSize<=SQLITE_MAX_PAGE_SIZE &&
        ((pageSize-1)&pageSize)==0 ){
    assert( (pageSize & 7)==0 );
    assert( !pBt->pCursor );
    if( nReserve>32 && pageSize==512 ) pageSize = 1024;
    pBt->pageSize = (u32)pageSize;
    freeTempSpace(pBt);
  }
  rc = sqlite3PagerSetPagesize(pBt->pPager, &pBt->pageSize, nReserve);
  pBt->usableSize = pBt->pageSize - (u16)nReserve;
  if( iFix ) pBt->btsFlags |= BTS_PAGESIZE_FIXED;
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** Return the currently defined page size
*/
int sqlite3BtreeGetPageSize(Btree *p){
  return p->pBt->pageSize;
}

/*
** This function is similar to sqlite3BtreeGetReserve(), except that it
** may only be called if it is guaranteed that the b-tree mutex is already
** held.
**
** This is useful in one special case in the backup API code where it is
** known that the shared b-tree mutex is held, but the mutex on the 
** database handle that owns *p is not. In this case if sqlite3BtreeEnter()
** were to be called, it might collide with some other operation on the
** database handle that owns *p, causing undefined behavior.
*/
int sqlite3BtreeGetReserveNoMutex(Btree *p){
  int n;
  assert( sqlite3_mutex_held(p->pBt->mutex) );
  n = p->pBt->pageSize - p->pBt->usableSize;
  return n;
}

/*
** Return the number of bytes of space at the end of every page that
** are intentually left unused.  This is the "reserved" space that is
** sometimes used by extensions.
**
** The value returned is the larger of the current reserve size and
** the latest reserve size requested by SQLITE_FILECTRL_RESERVE_BYTES.
** The amount of reserve can only grow - never shrink.
*/
int sqlite3BtreeGetRequestedReserve(Btree *p){
  int n1, n2;
  sqlite3BtreeEnter(p);
  n1 = (int)p->pBt->nReserveWanted;
  n2 = sqlite3BtreeGetReserveNoMutex(p);
  sqlite3BtreeLeave(p);
  return n1>n2 ? n1 : n2;
}


/*
** Set the maximum page count for a database if mxPage is positive.
** No changes are made if mxPage is 0 or negative.
** Regardless of the value of mxPage, return the maximum page count.
*/
Pgno sqlite3BtreeMaxPageCount(Btree *p, Pgno mxPage){
  Pgno n;
  sqlite3BtreeEnter(p);
  n = sqlite3PagerMaxPageCount(p->pBt->pPager, mxPage);
  sqlite3BtreeLeave(p);
  return n;
}

/*
** Change the values for the BTS_SECURE_DELETE and BTS_OVERWRITE flags:
**
**    newFlag==0       Both BTS_SECURE_DELETE and BTS_OVERWRITE are cleared
**    newFlag==1       BTS_SECURE_DELETE set and BTS_OVERWRITE is cleared
**    newFlag==2       BTS_SECURE_DELETE cleared and BTS_OVERWRITE is set
**    newFlag==(-1)    No changes
**
** This routine acts as a query if newFlag is less than zero
**
** With BTS_OVERWRITE set, deleted content is overwritten by zeros, but
** freelist leaf pages are not written back to the database.  Thus in-page
** deleted content is cleared, but freelist deleted content is not.
**
** With BTS_SECURE_DELETE, operation is like BTS_OVERWRITE with the addition
** that freelist leaf pages are written back into the database, increasing
** the amount of disk I/O.
*/
int sqlite3BtreeSecureDelete(Btree *p, int newFlag){
  int b;
  if( p==0 ) return 0;
  sqlite3BtreeEnter(p);
  assert( BTS_OVERWRITE==BTS_SECURE_DELETE*2 );
  assert( BTS_FAST_SECURE==(BTS_OVERWRITE|BTS_SECURE_DELETE) );
  if( newFlag>=0 ){
    p->pBt->btsFlags &= ~BTS_FAST_SECURE;
    p->pBt->btsFlags |= BTS_SECURE_DELETE*newFlag;
  }
  b = (p->pBt->btsFlags & BTS_FAST_SECURE)/BTS_SECURE_DELETE;
  sqlite3BtreeLeave(p);
  return b;
}

/*
** Change the 'auto-vacuum' property of the database. If the 'autoVacuum'
** parameter is non-zero, then auto-vacuum mode is enabled. If zero, it
** is disabled. The default value for the auto-vacuum property is 
** determined by the SQLITE_DEFAULT_AUTOVACUUM macro.
*/
int sqlite3BtreeSetAutoVacuum(Btree *p, int autoVacuum){
#ifdef SQLITE_OMIT_AUTOVACUUM
  return SQLITE_READONLY;
#else
  BtShared *pBt = p->pBt;
  int rc = SQLITE_OK;
  u8 av = (u8)autoVacuum;

  sqlite3BtreeEnter(p);
  if( (pBt->btsFlags & BTS_PAGESIZE_FIXED)!=0 && (av ?1:0)!=pBt->autoVacuum ){
    rc = SQLITE_READONLY;
  }else{
    pBt->autoVacuum = av ?1:0;
    pBt->incrVacuum = av==2 ?1:0;
  }
  sqlite3BtreeLeave(p);
  return rc;
#endif
}

/*
** Return the value of the 'auto-vacuum' property. If auto-vacuum is 
** enabled 1 is returned. Otherwise 0.
*/
int sqlite3BtreeGetAutoVacuum(Btree *p){
#ifdef SQLITE_OMIT_AUTOVACUUM
  return BTREE_AUTOVACUUM_NONE;
#else
  int rc;
  sqlite3BtreeEnter(p);
  rc = (
    (!p->pBt->autoVacuum)?BTREE_AUTOVACUUM_NONE:
    (!p->pBt->incrVacuum)?BTREE_AUTOVACUUM_FULL:
    BTREE_AUTOVACUUM_INCR
  );
  sqlite3BtreeLeave(p);
  return rc;
#endif
}

/*
** If the user has not set the safety-level for this database connection
** using "PRAGMA synchronous", and if the safety-level is not already
** set to the value passed to this function as the second parameter,
** set it so.
*/
#if SQLITE_DEFAULT_SYNCHRONOUS!=SQLITE_DEFAULT_WAL_SYNCHRONOUS \
    && !defined(SQLITE_OMIT_WAL)
static void setDefaultSyncFlag(BtShared *pBt, u8 safety_level){
  sqlite3 *db;
  Db *pDb;
  if( (db=pBt->db)!=0 && (pDb=db->aDb)!=0 ){
    while( pDb->pBt==0 || pDb->pBt->pBt!=pBt ){ pDb++; }
    if( pDb->bSyncSet==0 
     && pDb->safety_level!=safety_level 
     && pDb!=&db->aDb[1] 
    ){
      pDb->safety_level = safety_level;
      sqlite3PagerSetFlags(pBt->pPager,
          pDb->safety_level | (db->flags & PAGER_FLAGS_MASK));
    }
  }
}
#else
# define setDefaultSyncFlag(pBt,safety_level)
#endif

/* Forward declaration */
static int newDatabase(BtShared*);


/*
** Get a reference to pPage1 of the database file.  This will
** also acquire a readlock on that file.
**
** SQLITE_OK is returned on success.  If the file is not a
** well-formed database file, then SQLITE_CORRUPT is returned.
** SQLITE_BUSY is returned if the database is locked.  SQLITE_NOMEM
** is returned if we run out of memory. 
*/
static int lockBtree(BtShared *pBt){
  int rc;              /* Result code from subfunctions */
  MemPage *pPage1;     /* Page 1 of the database file */
  u32 nPage;           /* Number of pages in the database */
  u32 nPageFile = 0;   /* Number of pages in the database file */

  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( pBt->pPage1==0 );
  rc = sqlite3PagerSharedLock(pBt->pPager);
  if( rc!=SQLITE_OK ) return rc;
  rc = btreeGetPage(pBt, 1, &pPage1, 0);
  if( rc!=SQLITE_OK ) return rc;

  /* Do some checking to help insure the file we opened really is
  ** a valid database file. 
  */
  nPage = get4byte(28+(u8*)pPage1->aData);
  sqlite3PagerPagecount(pBt->pPager, (int*)&nPageFile);
  if( nPage==0 || memcmp(24+(u8*)pPage1->aData, 92+(u8*)pPage1->aData,4)!=0 ){
    nPage = nPageFile;
  }
  if( (pBt->db->flags & SQLITE_ResetDatabase)!=0 ){
    nPage = 0;
  }
  if( nPage>0 ){
    u32 pageSize;
    u32 usableSize;
    u8 *page1 = pPage1->aData;
    rc = SQLITE_NOTADB;
    /* EVIDENCE-OF: R-43737-39999 Every valid SQLite database file begins
    ** with the following 16 bytes (in hex): 53 51 4c 69 74 65 20 66 6f 72 6d
    ** 61 74 20 33 00. */
    if( memcmp(page1, zMagicHeader, 16)!=0 ){
      goto page1_init_failed;
    }

#ifdef SQLITE_OMIT_WAL
    if( page1[18]>1 ){
      pBt->btsFlags |= BTS_READ_ONLY;
    }
    if( page1[19]>1 ){
      goto page1_init_failed;
    }
#else
    if( page1[18]>2 ){
      pBt->btsFlags |= BTS_READ_ONLY;
    }
    if( page1[19]>2 ){
      goto page1_init_failed;
    }

    /* If the read version is set to 2, this database should be accessed
    ** in WAL mode. If the log is not already open, open it now. Then 
    ** return SQLITE_OK and return without populating BtShared.pPage1.
    ** The caller detects this and calls this function again. This is
    ** required as the version of page 1 currently in the page1 buffer
    ** may not be the latest version - there may be a newer one in the log
    ** file.
    */
    if( page1[19]==2 && (pBt->btsFlags & BTS_NO_WAL)==0 ){
      int isOpen = 0;
      rc = sqlite3PagerOpenWal(pBt->pPager, &isOpen);
      if( rc!=SQLITE_OK ){
        goto page1_init_failed;
      }else{
        setDefaultSyncFlag(pBt, SQLITE_DEFAULT_WAL_SYNCHRONOUS+1);
        if( isOpen==0 ){
          releasePageOne(pPage1);
          return SQLITE_OK;
        }
      }
      rc = SQLITE_NOTADB;
    }else{
      setDefaultSyncFlag(pBt, SQLITE_DEFAULT_SYNCHRONOUS+1);
    }
#endif

    /* EVIDENCE-OF: R-15465-20813 The maximum and minimum embedded payload
    ** fractions and the leaf payload fraction values must be 64, 32, and 32.
    **
    ** The original design allowed these amounts to vary, but as of
    ** version 3.6.0, we require them to be fixed.
    */
    if( memcmp(&page1[21], "\100\040\040",3)!=0 ){
      goto page1_init_failed;
    }
    /* EVIDENCE-OF: R-51873-39618 The page size for a database file is
    ** determined by the 2-byte integer located at an offset of 16 bytes from
    ** the beginning of the database file. */
    pageSize = (page1[16]<<8) | (page1[17]<<16);
    /* EVIDENCE-OF: R-25008-21688 The size of a page is a power of two
    ** between 512 and 65536 inclusive. */
    if( ((pageSize-1)&pageSize)!=0
     || pageSize>SQLITE_MAX_PAGE_SIZE 
     || pageSize<=256 
    ){
      goto page1_init_failed;
    }
    pBt->btsFlags |= BTS_PAGESIZE_FIXED;
    assert( (pageSize & 7)==0 );
    /* EVIDENCE-OF: R-59310-51205 The "reserved space" size in the 1-byte
    ** integer at offset 20 is the number of bytes of space at the end of
    ** each page to reserve for extensions. 
    **
    ** EVIDENCE-OF: R-37497-42412 The size of the reserved region is
    ** determined by the one-byte unsigned integer found at an offset of 20
    ** into the database file header. */
    usableSize = pageSize - page1[20];
    if( (u32)pageSize!=pBt->pageSize ){
      /* After reading the first page of the database assuming a page size
      ** of BtShared.pageSize, we have discovered that the page-size is
      ** actually pageSize. Unlock the database, leave pBt->pPage1 at
      ** zero and return SQLITE_OK. The caller will call this function
      ** again with the correct page-size.
      */
      releasePageOne(pPage1);
      pBt->usableSize = usableSize;
      pBt->pageSize = pageSize;
      freeTempSpace(pBt);
      rc = sqlite3PagerSetPagesize(pBt->pPager, &pBt->pageSize,
                                   pageSize-usableSize);
      return rc;
    }
    if( nPage>nPageFile ){
      if( sqlite3WritableSchema(pBt->db)==0 ){
        rc = SQLITE_CORRUPT_BKPT;
        goto page1_init_failed;
      }else{
        nPage = nPageFile;
      }
    }
    /* EVIDENCE-OF: R-28312-64704 However, the usable size is not allowed to
    ** be less than 480. In other words, if the page size is 512, then the
    ** reserved space size cannot exceed 32. */
    if( usableSize<480 ){
      goto page1_init_failed;
    }
    pBt->pageSize = pageSize;
    pBt->usableSize = usableSize;
#ifndef SQLITE_OMIT_AUTOVACUUM
    pBt->autoVacuum = (get4byte(&page1[36 + 4*4])?1:0);
    pBt->incrVacuum = (get4byte(&page1[36 + 7*4])?1:0);
#endif
  }

  /* maxLocal is the maximum amount of payload to store locally for
  ** a cell.  Make sure it is small enough so that at least minFanout
  ** cells can will fit on one page.  We assume a 10-byte page header.
  ** Besides the payload, the cell must store:
  **     2-byte pointer to the cell
  **     4-byte child pointer
  **     9-byte nKey value
  **     4-byte nData value
  **     4-byte overflow page pointer
  ** So a cell consists of a 2-byte pointer, a header which is as much as
  ** 17 bytes long, 0 to N bytes of payload, and an optional 4 byte overflow
  ** page pointer.
  */
  pBt->maxLocal = (u16)((pBt->usableSize-12)*64/255 - 23);
  pBt->minLocal = (u16)((pBt->usableSize-12)*32/255 - 23);
  pBt->maxLeaf = (u16)(pBt->usableSize - 35);
  pBt->minLeaf = (u16)((pBt->usableSize-12)*32/255 - 23);
  if( pBt->maxLocal>127 ){
    pBt->max1bytePayload = 127;
  }else{
    pBt->max1bytePayload = (u8)pBt->maxLocal;
  }
  assert( pBt->maxLeaf + 23 <= MX_CELL_SIZE(pBt) );
  pBt->pPage1 = pPage1;
  pBt->nPage = nPage;
  return SQLITE_OK;

page1_init_failed:
  releasePageOne(pPage1);
  pBt->pPage1 = 0;
  return rc;
}

#ifndef NDEBUG
/*
** Return the number of cursors open on pBt. This is for use
** in assert() expressions, so it is only compiled if NDEBUG is not
** defined.
**
** Only write cursors are counted if wrOnly is true.  If wrOnly is
** false then all cursors are counted.
**
** For the purposes of this routine, a cursor is any cursor that
** is capable of reading or writing to the database.  Cursors that
** have been tripped into the CURSOR_FAULT state are not counted.
*/
static int countValidCursors(BtShared *pBt, int wrOnly){
  BtCursor *pCur;
  int r = 0;
  for(pCur=pBt->pCursor; pCur; pCur=pCur->pNext){
    if( (wrOnly==0 || (pCur->curFlags & BTCF_WriteFlag)!=0)
     && pCur->eState!=CURSOR_FAULT ) r++; 
  }
  return r;
}
#endif

/*
** If there are no outstanding cursors and we are not in the middle
** of a transaction but there is a read lock on the database, then
** this routine unrefs the first page of the database file which 
** has the effect of releasing the read lock.
**
** If there is a transaction in progress, this routine is a no-op.
*/
static void unlockBtreeIfUnused(BtShared *pBt){
  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( countValidCursors(pBt,0)==0 || pBt->inTransaction>TRANS_NONE );
  if( pBt->inTransaction==TRANS_NONE && pBt->pPage1!=0 ){
    MemPage *pPage1 = pBt->pPage1;
    assert( pPage1->aData );
    assert( sqlite3PagerRefcount(pBt->pPager)==1 );
    pBt->pPage1 = 0;
    releasePageOne(pPage1);
  }
}

/*
** If pBt points to an empty file then convert that empty file
** into a new empty database by initializing the first page of
** the database.
*/
static int newDatabase(BtShared *pBt){
  MemPage *pP1;
  unsigned char *data;
  int rc;

  assert( sqlite3_mutex_held(pBt->mutex) );
  if( pBt->nPage>0 ){
    return SQLITE_OK;
  }
  pP1 = pBt->pPage1;
  assert( pP1!=0 );
  data = pP1->aData;
  rc = sqlite3PagerWrite(pP1->pDbPage);
  if( rc ) return rc;
  memcpy(data, zMagicHeader, sizeof(zMagicHeader));
  assert( sizeof(zMagicHeader)==16 );
  data[16] = (u8)((pBt->pageSize>>8)&0xff);
  data[17] = (u8)((pBt->pageSize>>16)&0xff);
  data[18] = 1;
  data[19] = 1;
  assert( pBt->usableSize<=pBt->pageSize && pBt->usableSize+255>=pBt->pageSize);
  data[20] = (u8)(pBt->pageSize - pBt->usableSize);
  data[21] = 64;
  data[22] = 32;
  data[23] = 32;
  memset(&data[24], 0, 100-24);
  zeroPage(pP1, PTF_INTKEY|PTF_LEAF|PTF_LEAFDATA );
  pBt->btsFlags |= BTS_PAGESIZE_FIXED;
#ifndef SQLITE_OMIT_AUTOVACUUM
  assert( pBt->autoVacuum==1 || pBt->autoVacuum==0 );
  assert( pBt->incrVacuum==1 || pBt->incrVacuum==0 );
  put4byte(&data[36 + 4*4], pBt->autoVacuum);
  put4byte(&data[36 + 7*4], pBt->incrVacuum);
#endif
  pBt->nPage = 1;
  data[31] = 1;
  return SQLITE_OK;
}

/*
** Initialize the first page of the database file (creating a database
** consisting of a single page and no schema objects). Return SQLITE_OK
** if successful, or an SQLite error code otherwise.
*/
int sqlite3BtreeNewDb(Btree *p){
  int rc;
  sqlite3BtreeEnter(p);
  p->pBt->nPage = 0;
  rc = newDatabase(p->pBt);
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** Attempt to start a new transaction. A write-transaction
** is started if the second argument is nonzero, otherwise a read-
** transaction.  If the second argument is 2 or more and exclusive
** transaction is started, meaning that no other process is allowed
** to access the database.  A preexisting transaction may not be
** upgraded to exclusive by calling this routine a second time - the
** exclusivity flag only works for a new transaction.
**
** A write-transaction must be started before attempting any 
** changes to the database.  None of the following routines 
** will work unless a transaction is started first:
**
**      sqlite3BtreeCreateTable()
**      sqlite3BtreeCreateIndex()
**      sqlite3BtreeClearTable()
**      sqlite3BtreeDropTable()
**      sqlite3BtreeInsert()
**      sqlite3BtreeDelete()
**      sqlite3BtreeUpdateMeta()
**
** If an initial attempt to acquire the lock fails because of lock contention
** and the database was previously unlocked, then invoke the busy handler
** if there is one.  But if there was previously a read-lock, do not
** invoke the busy handler - just return SQLITE_BUSY.  SQLITE_BUSY is 
** returned when there is already a read-lock in order to avoid a deadlock.
**
** Suppose there are two processes A and B.  A has a read lock and B has
** a reserved lock.  B tries to promote to exclusive but is blocked because
** of A's read lock.  A tries to promote to reserved but is blocked by B.
** One or the other of the two processes must give way or there can be
** no progress.  By returning SQLITE_BUSY and not invoking the busy callback
** when A already has a read lock, we encourage A to give up and let B
** proceed.
*/
int sqlite3BtreeBeginTrans(Btree *p, int wrflag, int *pSchemaVersion){
  BtShared *pBt = p->pBt;
  Pager *pPager = pBt->pPager;
  int rc = SQLITE_OK;

  sqlite3BtreeEnter(p);
  btreeIntegrity(p);

  /* If the btree is already in a write-transaction, or it
  ** is already in a read-transaction and a read-transaction
  ** is requested, this is a no-op.
  */
  if( p->inTrans==TRANS_WRITE || (p->inTrans==TRANS_READ && !wrflag) ){
    goto trans_begun;
  }
  assert( pBt->inTransaction==TRANS_WRITE || IfNotOmitAV(pBt->bDoTruncate)==0 );

  if( (p->db->flags & SQLITE_ResetDatabase) 
   && sqlite3PagerIsreadonly(pPager)==0 
  ){
    pBt->btsFlags &= ~BTS_READ_ONLY;
  }

  /* Write transactions are not possible on a read-only database */
  if( (pBt->btsFlags & BTS_READ_ONLY)!=0 && wrflag ){
    rc = SQLITE_READONLY;
    goto trans_begun;
  }

#ifndef SQLITE_OMIT_SHARED_CACHE
  {
    sqlite3 *pBlock = 0;
    /* If another database handle has already opened a write transaction 
    ** on this shared-btree structure and a second write transaction is
    ** requested, return SQLITE_LOCKED.
    */
    if( (wrflag && pBt->inTransaction==TRANS_WRITE)
     || (pBt->btsFlags & BTS_PENDING)!=0
    ){
      pBlock = pBt->pWriter->db;
    }else if( wrflag>1 ){
      BtLock *pIter;
      for(pIter=pBt->pLock; pIter; pIter=pIter->pNext){
        if( pIter->pBtree!=p ){
          pBlock = pIter->pBtree->db;
          break;
        }
      }
    }
    if( pBlock ){
      sqlite3ConnectionBlocked(p->db, pBlock);
      rc = SQLITE_LOCKED_SHAREDCACHE;
      goto trans_begun;
    }
  }
#endif

  /* Any read-only or read-write transaction implies a read-lock on 
  ** page 1. So if some other shared-cache client already has a write-lock 
  ** on page 1, the transaction cannot be opened. */
  rc = querySharedCacheTableLock(p, SCHEMA_ROOT, READ_LOCK);
  if( SQLITE_OK!=rc ) goto trans_begun;

  pBt->btsFlags &= ~BTS_INITIALLY_EMPTY;
  if( pBt->nPage==0 ) pBt->btsFlags |= BTS_INITIALLY_EMPTY;
  do {
    sqlite3PagerWalDb(pPager, p->db);

#ifdef SQLITE_ENABLE_SETLK_TIMEOUT
    /* If transitioning from no transaction directly to a write transaction,
    ** block for the WRITER lock first if possible. */
    if( pBt->pPage1==0 && wrflag ){
      assert( pBt->inTransaction==TRANS_NONE );
      rc = sqlite3PagerWalWriteLock(pPager, 1);
      if( rc!=SQLITE_BUSY && rc!=SQLITE_OK ) break;
    }
#endif

    /* Call lockBtree() until either pBt->pPage1 is populated or
    ** lockBtree() returns something other than SQLITE_OK. lockBtree()
    ** may return SQLITE_OK but leave pBt->pPage1 set to 0 if after
    ** reading page 1 it discovers that the page-size of the database 
    ** file is not pBt->pageSize. In this case lockBtree() will update
    ** pBt->pageSize to the page-size of the file on disk.
    */
    while( pBt->pPage1==0 && SQLITE_OK==(rc = lockBtree(pBt)) );

    if( rc==SQLITE_OK && wrflag ){
      if( (pBt->btsFlags & BTS_READ_ONLY)!=0 ){
        rc = SQLITE_READONLY;
      }else{
        rc = sqlite3PagerBegin(pPager, wrflag>1, sqlite3TempInMemory(p->db));
        if( rc==SQLITE_OK ){
          rc = newDatabase(pBt);
        }else if( rc==SQLITE_BUSY_SNAPSHOT && pBt->inTransaction==TRANS_NONE ){
          /* if there was no transaction opened when this function was
          ** called and SQLITE_BUSY_SNAPSHOT is returned, change the error
          ** code to SQLITE_BUSY. */
          rc = SQLITE_BUSY;
        }
      }
    }
  
    if( rc!=SQLITE_OK ){
      (void)sqlite3PagerWalWriteLock(pPager, 0);
      unlockBtreeIfUnused(pBt);
    }
  }while( (rc&0xFF)==SQLITE_BUSY && pBt->inTransaction==TRANS_NONE &&
          btreeInvokeBusyHandler(pBt) );
  sqlite3PagerWalDb(pPager, 0);
#ifdef SQLITE_ENABLE_SETLK_TIMEOUT
  if( rc==SQLITE_BUSY_TIMEOUT ) rc = SQLITE_BUSY;
#endif

  if( rc==SQLITE_OK ){
    if( p->inTrans==TRANS_NONE ){
      pBt->nTransaction++;
#ifndef SQLITE_OMIT_SHARED_CACHE
      if( p->sharable ){
        assert( p->lock.pBtree==p && p->lock.iTable==1 );
        p->lock.eLock = READ_LOCK;
        p->lock.pNext = pBt->pLock;
        pBt->pLock = &p->lock;
      }
#endif
    }
    p->inTrans = (wrflag?TRANS_WRITE:TRANS_READ);
    if( p->inTrans>pBt->inTransaction ){
      pBt->inTransaction = p->inTrans;
    }
    if( wrflag ){
      MemPage *pPage1 = pBt->pPage1;
#ifndef SQLITE_OMIT_SHARED_CACHE
      assert( !pBt->pWriter );
      pBt->pWriter = p;
      pBt->btsFlags &= ~BTS_EXCLUSIVE;
      if( wrflag>1 ) pBt->btsFlags |= BTS_EXCLUSIVE;
#endif

      /* If the db-size header field is incorrect (as it may be if an old
      ** client has been writing the database file), update it now. Doing
      ** this sooner rather than later means the database size can safely 
      ** re-read the database size from page 1 if a savepoint or transaction
      ** rollback occurs within the transaction.
      */
      if( pBt->nPage!=get4byte(&pPage1->aData[28]) ){
        rc = sqlite3PagerWrite(pPage1->pDbPage);
        if( rc==SQLITE_OK ){
          put4byte(&pPage1->aData[28], pBt->nPage);
        }
      }
    }
  }

trans_begun:
  if( rc==SQLITE_OK ){
    if( pSchemaVersion ){
      *pSchemaVersion = get4byte(&pBt->pPage1->aData[40]);
    }
    if( wrflag ){
      /* This call makes sure that the pager has the correct number of
      ** open savepoints. If the second parameter is greater than 0 and
      ** the sub-journal is not already open, then it will be opened here.
      */
      rc = sqlite3PagerOpenSavepoint(pPager, p->db->nSavepoint);
    }
  }

  btreeIntegrity(p);
  sqlite3BtreeLeave(p);
  return rc;
}

#ifndef SQLITE_OMIT_AUTOVACUUM

/*
** Set the pointer-map entries for all children of page pPage. Also, if
** pPage contains cells that point to overflow pages, set the pointer
** map entries for the overflow pages as well.
*/
static int setChildPtrmaps(MemPage *pPage){
  int i;                             /* Counter variable */
  int nCell;                         /* Number of cells in page pPage */
  int rc;                            /* Return code */
  BtShared *pBt = pPage->pBt;
  Pgno pgno = pPage->pgno;

  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  rc = pPage->isInit ? SQLITE_OK : btreeInitPage(pPage);
  if( rc!=SQLITE_OK ) return rc;
  nCell = pPage->nCell;

  for(i=0; i<nCell; i++){
    u8 *pCell = findCell(pPage, i);

    ptrmapPutOvflPtr(pPage, pPage, pCell, &rc);

    if( !pPage->leaf ){
      Pgno childPgno = get4byte(pCell);
      ptrmapPut(pBt, childPgno, PTRMAP_BTREE, pgno, &rc);
    }
  }

  if( !pPage->leaf ){
    Pgno childPgno = get4byte(&pPage->aData[pPage->hdrOffset+8]);
    ptrmapPut(pBt, childPgno, PTRMAP_BTREE, pgno, &rc);
  }

  return rc;
}

/*
** Somewhere on pPage is a pointer to page iFrom.  Modify this pointer so
** that it points to iTo. Parameter eType describes the type of pointer to
** be modified, as  follows:
**
** PTRMAP_BTREE:     pPage is a btree-page. The pointer points at a child 
**                   page of pPage.
**
** PTRMAP_OVERFLOW1: pPage is a btree-page. The pointer points at an overflow
**                   page pointed to by one of the cells on pPage.
**
** PTRMAP_OVERFLOW2: pPage is an overflow-page. The pointer points at the next
**                   overflow page in the list.
*/
static int modifyPagePointer(MemPage *pPage, Pgno iFrom, Pgno iTo, u8 eType){
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( sqlite3PagerIswriteable(pPage->pDbPage) );
  if( eType==PTRMAP_OVERFLOW2 ){
    /* The pointer is always the first 4 bytes of the page in this case.  */
    if( get4byte(pPage->aData)!=iFrom ){
      return SQLITE_CORRUPT_PAGE(pPage);
    }
    put4byte(pPage->aData, iTo);
  }else{
    int i;
    int nCell;
    int rc;

    rc = pPage->isInit ? SQLITE_OK : btreeInitPage(pPage);
    if( rc ) return rc;
    nCell = pPage->nCell;

    for(i=0; i<nCell; i++){
      u8 *pCell = findCell(pPage, i);
      if( eType==PTRMAP_OVERFLOW1 ){
        CellInfo info;
        pPage->xParseCell(pPage, pCell, &info);
        if( info.nLocal<info.nPayload ){
          if( pCell+info.nSize > pPage->aData+pPage->pBt->usableSize ){
            return SQLITE_CORRUPT_PAGE(pPage);
          }
          if( iFrom==get4byte(pCell+info.nSize-4) ){
            put4byte(pCell+info.nSize-4, iTo);
            break;
          }
        }
      }else{
        if( pCell+4 > pPage->aData+pPage->pBt->usableSize ){
          return SQLITE_CORRUPT_PAGE(pPage);
        }
        if( get4byte(pCell)==iFrom ){
          put4byte(pCell, iTo);
          break;
        }
      }
    }
  
    if( i==nCell ){
      if( eType!=PTRMAP_BTREE || 
          get4byte(&pPage->aData[pPage->hdrOffset+8])!=iFrom ){
        return SQLITE_CORRUPT_PAGE(pPage);
      }
      put4byte(&pPage->aData[pPage->hdrOffset+8], iTo);
    }
  }
  return SQLITE_OK;
}


/*
** 将已打开的数据库页 pDbPage 移动到数据库中的 iFreePage 位置。
** 移动后 pDbPage 的引用仍然有效。
**
** isCommit 标志表示无需记录在写入数据库页 pDbPage->pgno 前需要 sync() 同步日志。
** 调用方已承诺不会写入该页。
*/
static int relocatePage(
  BtShared *pBt,           /* Btree */
  MemPage *pDbPage,        /* Open page to move */
  u8 eType,                /* Pointer map 'type' entry for pDbPage */
  Pgno iPtrPage,           /* Pointer map 'page-no' entry for pDbPage */
  Pgno iFreePage,          /* The location to move pDbPage to */
  int isCommit             /* isCommit flag passed to sqlite3PagerMovepage */
){
  MemPage *pPtrPage;   /* The page that contains a pointer to pDbPage */
  Pgno iDbPage = pDbPage->pgno;
  Pager *pPager = pBt->pPager;
  int rc;

  assert( eType==PTRMAP_OVERFLOW2 || eType==PTRMAP_OVERFLOW1 || 
      eType==PTRMAP_BTREE || eType==PTRMAP_ROOTPAGE );
  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( pDbPage->pBt==pBt );
  if( iDbPage<3 ) return SQLITE_CORRUPT_BKPT;

  /* Move page iDbPage from its current location to page number iFreePage */
  TRACE(("AUTOVACUUM: Moving %d to free page %d (ptr page %d type %d)\n", 
      iDbPage, iFreePage, iPtrPage, eType));
  rc = sqlite3PagerMovepage(pPager, pDbPage->pDbPage, iFreePage, isCommit);
  if( rc!=SQLITE_OK ){
    return rc;
  }
  pDbPage->pgno = iFreePage;

  /* If pDbPage was a btree-page, then it may have child pages and/or cells
  ** that point to overflow pages. The pointer map entries for all these
  ** pages need to be changed.
  **
  ** If pDbPage is an overflow page, then the first 4 bytes may store a
  ** pointer to a subsequent overflow page. If this is the case, then
  ** the pointer map needs to be updated for the subsequent overflow page.
  */
  if( eType==PTRMAP_BTREE || eType==PTRMAP_ROOTPAGE ){
    rc = setChildPtrmaps(pDbPage);
    if( rc!=SQLITE_OK ){
      return rc;
    }
  }else{
    Pgno nextOvfl = get4byte(pDbPage->aData);
    if( nextOvfl!=0 ){
      ptrmapPut(pBt, nextOvfl, PTRMAP_OVERFLOW2, iFreePage, &rc);
      if( rc!=SQLITE_OK ){
        return rc;
      }
    }
  }

  /* Fix the database pointer on page iPtrPage that pointed at iDbPage so
  ** that it points at iFreePage. Also fix the pointer map entry for
  ** iPtrPage.
  */
  if( eType!=PTRMAP_ROOTPAGE ){
    rc = btreeGetPage(pBt, iPtrPage, &pPtrPage, 0);
    if( rc!=SQLITE_OK ){
      return rc;
    }
    rc = sqlite3PagerWrite(pPtrPage->pDbPage);
    if( rc!=SQLITE_OK ){
      releasePage(pPtrPage);
      return rc;
    }
    rc = modifyPagePointer(pPtrPage, iDbPage, iFreePage, eType);
    releasePage(pPtrPage);
    if( rc==SQLITE_OK ){
      ptrmapPut(pBt, iFreePage, eType, iPtrPage, &rc);
    }
  }
  return rc;
}

/* Forward declaration required by incrVacuumStep(). */
static int allocateBtreePage(BtShared *, MemPage **, Pgno *, Pgno, u8);

/*
** 执行增量清理（incremental-vacuum）的一个步骤。若成功，返回 SQLITE_OK；
** 若无后续工作可做（即无需再次调用此函数），返回 SQLITE_DONE；
** 若发生错误，返回其他错误码。
**
** 具体而言，此函数尝试重新组织数据库，使得当前已使用的文件最后一页变为未使用状态。
**
** 参数 nFin 表示若持续调用此函数直至返回 SQLITE_DONE 时，数据库将包含的页数。
**
** 若参数 bCommit 为非零值，此函数假设调用方将持续调用 incrVacuumStep()，
** 直至其返回 SQLITE_DONE 或错误。bCommit 为 true 表示这是提交时的自动清理操作，
** 为 false 则表示普通的增量清理。
*/
static int incrVacuumStep(BtShared *pBt, Pgno nFin, Pgno iLastPg, int bCommit){
  Pgno nFreeList;           /* Number of pages still on the free-list */
  int rc;

  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( iLastPg>nFin );

  if( !PTRMAP_ISPAGE(pBt, iLastPg) && iLastPg!=PENDING_BYTE_PAGE(pBt) ){
    u8 eType;
    Pgno iPtrPage;

    nFreeList = get4byte(&pBt->pPage1->aData[36]);
    if( nFreeList==0 ){
      return SQLITE_DONE;
    }

    rc = ptrmapGet(pBt, iLastPg, &eType, &iPtrPage);
    if( rc!=SQLITE_OK ){
      return rc;
    }
    if( eType==PTRMAP_ROOTPAGE ){
      return SQLITE_CORRUPT_BKPT;
    }

    if( eType==PTRMAP_FREEPAGE ){
      if( bCommit==0 ){
        /* 
        ** 将该页面从文件的空闲列表（free-list）中移除。若 bCommit 参数非零，则无需执行此操作。
        ** 因为在这种情况下，函数返回后空闲列表将被清空（截断至零长度），即使列表中仍残留部分无效条目也无影响。
        */
        Pgno iFreePg;
        MemPage *pFreePg;
        rc = allocateBtreePage(pBt, &pFreePg, &iFreePg, iLastPg, BTALLOC_EXACT);
        if( rc!=SQLITE_OK ){
          return rc;
        }
        assert( iFreePg==iLastPg );
        releasePage(pFreePg);
      }
    } else {
      Pgno iFreePg;             /* Index of free page to move pLastPg to */
      MemPage *pLastPg;
      u8 eMode = BTALLOC_ANY;   /* Mode parameter for allocateBtreePage() */
      Pgno iNear = 0;           /* nearby parameter for allocateBtreePage() */

      rc = btreeGetPage(pBt, iLastPg, &pLastPg, 0);
      if( rc!=SQLITE_OK ){
        return rc;
      }

      /* 
      ** 若 bCommit 为 0，此循环仅执行一次，并将页面 pLastPg 与从空闲列表取出的首个空闲页进行交换。
      ** 反之，若 bCommit 大于 0，则持续循环直至找到位于文件前 nFin 页内的空闲页。
      */
      if( bCommit==0 ){
        eMode = BTALLOC_LE;
        iNear = nFin;
      }
      do {
        MemPage *pFreePg;
        Pgno dbSize = btreePagecount(pBt);
        rc = allocateBtreePage(pBt, &pFreePg, &iFreePg, iNear, eMode);
        if( rc!=SQLITE_OK ){
          releasePage(pLastPg);
          return rc;
        }
        releasePage(pFreePg);
        if( iFreePg>dbSize ){
          releasePage(pLastPg);
          return SQLITE_CORRUPT_BKPT;
        }
      }while( bCommit && iFreePg>nFin );
      assert( iFreePg<iLastPg );
      
      rc = relocatePage(pBt, pLastPg, eType, iPtrPage, iFreePg, bCommit);
      releasePage(pLastPg);
      if( rc!=SQLITE_OK ){
        return rc;
      }
    }
  }

  if( bCommit==0 ){
    do {
      iLastPg--;
    }while( iLastPg==PENDING_BYTE_PAGE(pBt) || PTRMAP_ISPAGE(pBt, iLastPg) );
    pBt->bDoTruncate = 1;
    pBt->nPage = iLastPg;
  }
  return SQLITE_OK;
}

/*
** The database opened by the first argument is an auto-vacuum database
** nOrig pages in size containing nFree free pages. Return the expected 
** size of the database in pages following an auto-vacuum operation.
*/
static Pgno finalDbSize(BtShared *pBt, Pgno nOrig, Pgno nFree){
  int nEntry;                     /* Number of entries on one ptrmap page */
  Pgno nPtrmap;                   /* Number of PtrMap pages to be freed */
  Pgno nFin;                      /* Return value */

  nEntry = pBt->usableSize/5;
  nPtrmap = (nFree-nOrig+PTRMAP_PAGENO(pBt, nOrig)+nEntry)/nEntry;
  nFin = nOrig - nFree - nPtrmap;
  if( nOrig>PENDING_BYTE_PAGE(pBt) && nFin<PENDING_BYTE_PAGE(pBt) ){
    nFin--;
  }
  while( PTRMAP_ISPAGE(pBt, nFin) || nFin==PENDING_BYTE_PAGE(pBt) ){
    nFin--;
  }

  return nFin;
}

/*
** 调用此函数前必须已开启一个写事务。
** 该函数执行增量清理（incremental vacuum）的一个工作单元。
**
** 若此函数运行后增量清理已完成，则返回 SQLITE_DONE；
** 若未完成但未发生错误，返回 SQLITE_OK；
** 否则返回 SQLite 错误码。
*/
int sqlite3BtreeIncrVacuum(Btree *p){
  int rc;
  BtShared *pBt = p->pBt;

  sqlite3BtreeEnter(p);
  assert( pBt->inTransaction==TRANS_WRITE && p->inTrans==TRANS_WRITE );
  if( !pBt->autoVacuum ){
    rc = SQLITE_DONE;
  }else{
    Pgno nOrig = btreePagecount(pBt);
    Pgno nFree = get4byte(&pBt->pPage1->aData[36]);
    Pgno nFin = finalDbSize(pBt, nOrig, nFree);

    if( nOrig<nFin || nFree>=nOrig ){
      rc = SQLITE_CORRUPT_BKPT;
    }else if( nFree>0 ){
      rc = saveAllCursors(pBt, 0, 0);
      if( rc==SQLITE_OK ){
        invalidateAllOverflowCache(pBt);
        rc = incrVacuumStep(pBt, nFin, nOrig, 0);
      }
      if( rc==SQLITE_OK ){
        rc = sqlite3PagerWrite(pBt->pPage1->pDbPage);
        put4byte(&pBt->pPage1->aData[28], pBt->nPage);
      }
    }else{
      rc = SQLITE_DONE;
    }
  }
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** This routine is called prior to sqlite3PagerCommit when a transaction
** is committed for an auto-vacuum database.
*/
static int autoVacuumCommit(Btree *p){
  int rc = SQLITE_OK;
  Pager *pPager;
  BtShared *pBt;
  sqlite3 *db;
  VVA_ONLY( int nRef );

  assert( p!=0 );
  pBt = p->pBt;  
  pPager = pBt->pPager;
  VVA_ONLY( nRef = sqlite3PagerRefcount(pPager); )

  assert( sqlite3_mutex_held(pBt->mutex) );
  invalidateAllOverflowCache(pBt);
  assert(pBt->autoVacuum);
  if( !pBt->incrVacuum ){
    Pgno nFin;         /* Number of pages in database after autovacuuming */
    Pgno nFree;        /* Number of pages on the freelist initially */
    Pgno nVac;         /* Number of pages to vacuum */
    Pgno iFree;        /* The next page to be freed */
    Pgno nOrig;        /* Database size before freeing */

    nOrig = btreePagecount(pBt);
    if( PTRMAP_ISPAGE(pBt, nOrig) || nOrig==PENDING_BYTE_PAGE(pBt) ){
      /* It is not possible to create a database for which the final page
      ** is either a pointer-map page or the pending-byte page. If one
      ** is encountered, this indicates corruption.
      */
      return SQLITE_CORRUPT_BKPT;
    }

    nFree = get4byte(&pBt->pPage1->aData[36]);
    db = p->db;
    if( db->xAutovacPages ){
      int iDb;
      for(iDb=0; ALWAYS(iDb<db->nDb); iDb++){
        if( db->aDb[iDb].pBt==p ) break;
      }
      nVac = db->xAutovacPages(
        db->pAutovacPagesArg,
        db->aDb[iDb].zDbSName,
        nOrig,
        nFree,
        pBt->pageSize
      );
      if( nVac>nFree ){
        nVac = nFree;
      }
      if( nVac==0 ){
        return SQLITE_OK;
      }
    }else{
      nVac = nFree;
    }
    nFin = finalDbSize(pBt, nOrig, nVac);
    if( nFin>nOrig ) return SQLITE_CORRUPT_BKPT;
    if( nFin<nOrig ){
      rc = saveAllCursors(pBt, 0, 0);
    }
    for(iFree=nOrig; iFree>nFin && rc==SQLITE_OK; iFree--){
      rc = incrVacuumStep(pBt, nFin, iFree, nVac==nFree);
    }
    if( (rc==SQLITE_DONE || rc==SQLITE_OK) && nFree>0 ){
      rc = sqlite3PagerWrite(pBt->pPage1->pDbPage);
      if( nVac==nFree ){
        put4byte(&pBt->pPage1->aData[32], 0);
        put4byte(&pBt->pPage1->aData[36], 0);
      }
      put4byte(&pBt->pPage1->aData[28], nFin);
      pBt->bDoTruncate = 1;
      pBt->nPage = nFin;
    }
    if( rc!=SQLITE_OK ){
      sqlite3PagerRollback(pPager);
    }
  }

  assert( nRef>=sqlite3PagerRefcount(pPager) );
  return rc;
}

#else /* ifndef SQLITE_OMIT_AUTOVACUUM */
# define setChildPtrmaps(x) SQLITE_OK
#endif

/*
** 此函数执行两阶段提交的第一阶段。若当前不存在回滚日志，
** 该过程会创建一个回滚日志，并填充足够的信息以确保在断电情况下，
** 通过回放日志可将数据库恢复到原始状态。随后，日志内容会被刷新到磁盘。
** 当日志安全落盘后，数据库的变更将写入数据库文件并刷新到磁盘。
** 此函数结束时，回滚日志仍存在于磁盘上且所有锁仍被持有，因此事务尚未提交。
** 提交过程的第二阶段请参见 sqlite3BtreeCommitPhaseTwo()。
**
** 如果当前在 pBt 上没有活动的写事务，此调用为空操作。
**
** 否则，同步 pBt 对应的数据库文件。zSuperJrnl 参数指向应写入单个日志文件的
** 超级日志文件名称，若为 NULL 则表示无超级日志文件（单数据库事务）。
**
** 调用此函数时，超级日志应已创建并填充了该日志指针，且已同步到磁盘。
**
** 此函数返回后，提交该数据库文件写事务的唯一操作是删除日志文件。
*/
int sqlite3BtreeCommitPhaseOne(Btree *p, const char *zSuperJrnl){
  int rc = SQLITE_OK;
  if( p->inTrans==TRANS_WRITE ){
    BtShared *pBt = p->pBt;
    sqlite3BtreeEnter(p);
#ifndef SQLITE_OMIT_AUTOVACUUM
    if( pBt->autoVacuum ){
      rc = autoVacuumCommit(p);
      if( rc!=SQLITE_OK ){
        sqlite3BtreeLeave(p);
        return rc;
      }
    }
    if( pBt->bDoTruncate ){
      sqlite3PagerTruncateImage(pBt->pPager, pBt->nPage);
    }
#endif
    rc = sqlite3PagerCommitPhaseOne(pBt->pPager, zSuperJrnl, 0);
    sqlite3BtreeLeave(p);
  }
  return rc;
}

/*
** This function is called from both BtreeCommitPhaseTwo() and BtreeRollback()
** at the conclusion of a transaction.
*/
static void btreeEndTransaction(Btree *p){
  BtShared *pBt = p->pBt;
  sqlite3 *db = p->db;
  assert( sqlite3BtreeHoldsMutex(p) );

#ifndef SQLITE_OMIT_AUTOVACUUM
  pBt->bDoTruncate = 0;
#endif
  if( p->inTrans>TRANS_NONE && db->nVdbeRead>1 ){
    /* If there are other active statements that belong to this database
    ** handle, downgrade to a read-only transaction. The other statements
    ** may still be reading from the database.  */
    downgradeAllSharedCacheTableLocks(p);
    p->inTrans = TRANS_READ;
  }else{
    /* If the handle had any kind of transaction open, decrement the 
    ** transaction count of the shared btree. If the transaction count 
    ** reaches 0, set the shared state to TRANS_NONE. The unlockBtreeIfUnused()
    ** call below will unlock the pager.  */
    if( p->inTrans!=TRANS_NONE ){
      clearAllSharedCacheTableLocks(p);
      pBt->nTransaction--;
      if( 0==pBt->nTransaction ){
        pBt->inTransaction = TRANS_NONE;
      }
    }

    /* Set the current transaction state to TRANS_NONE and unlock the 
    ** pager if this call closed the only read or write transaction.  */
    p->inTrans = TRANS_NONE;
    unlockBtreeIfUnused(pBt);
  }

  btreeIntegrity(p);
}

/*
** Commit the transaction currently in progress.
**
** This routine implements the second phase of a 2-phase commit.  The
** sqlite3BtreeCommitPhaseOne() routine does the first phase and should
** be invoked prior to calling this routine.  The sqlite3BtreeCommitPhaseOne()
** routine did all the work of writing information out to disk and flushing the
** contents so that they are written onto the disk platter.  All this
** routine has to do is delete or truncate or zero the header in the
** the rollback journal (which causes the transaction to commit) and
** drop locks.
**
** Normally, if an error occurs while the pager layer is attempting to 
** finalize the underlying journal file, this function returns an error and
** the upper layer will attempt a rollback. However, if the second argument
** is non-zero then this b-tree transaction is part of a multi-file 
** transaction. In this case, the transaction has already been committed 
** (by deleting a super-journal file) and the caller will ignore this 
** functions return code. So, even if an error occurs in the pager layer,
** reset the b-tree objects internal state to indicate that the write
** transaction has been closed. This is quite safe, as the pager will have
** transitioned to the error state.
**
** This will release the write lock on the database file.  If there
** are no active cursors, it also releases the read lock.
*/
int sqlite3BtreeCommitPhaseTwo(Btree *p, int bCleanup){

  if( p->inTrans==TRANS_NONE ) return SQLITE_OK;
  sqlite3BtreeEnter(p);
  btreeIntegrity(p);

  /* If the handle has a write-transaction open, commit the shared-btrees 
  ** transaction and set the shared state to TRANS_READ.
  */
  if( p->inTrans==TRANS_WRITE ){
    int rc;
    BtShared *pBt = p->pBt;
    assert( pBt->inTransaction==TRANS_WRITE );
    assert( pBt->nTransaction>0 );
    rc = sqlite3PagerCommitPhaseTwo(pBt->pPager);
    if( rc!=SQLITE_OK && bCleanup==0 ){
      sqlite3BtreeLeave(p);
      return rc;
    }
    p->iBDataVersion--;  /* Compensate for pPager->iDataVersion++; */
    pBt->inTransaction = TRANS_READ;
    btreeClearHasContent(pBt);
  }

  btreeEndTransaction(p);
  sqlite3BtreeLeave(p);
  return SQLITE_OK;
}

/*
** Do both phases of a commit.
*/
int sqlite3BtreeCommit(Btree *p){
  int rc;
  sqlite3BtreeEnter(p);
  rc = sqlite3BtreeCommitPhaseOne(p, 0);
  if( rc==SQLITE_OK ){
    rc = sqlite3BtreeCommitPhaseTwo(p, 0);
  }
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** This routine sets the state to CURSOR_FAULT and the error
** code to errCode for every cursor on any BtShared that pBtree
** references.  Or if the writeOnly flag is set to 1, then only
** trip write cursors and leave read cursors unchanged.
**
** Every cursor is a candidate to be tripped, including cursors
** that belong to other database connections that happen to be
** sharing the cache with pBtree.
**
** This routine gets called when a rollback occurs. If the writeOnly
** flag is true, then only write-cursors need be tripped - read-only
** cursors save their current positions so that they may continue 
** following the rollback. Or, if writeOnly is false, all cursors are 
** tripped. In general, writeOnly is false if the transaction being
** rolled back modified the database schema. In this case b-tree root
** pages may be moved or deleted from the database altogether, making
** it unsafe for read cursors to continue.
**
** If the writeOnly flag is true and an error is encountered while 
** saving the current position of a read-only cursor, all cursors, 
** including all read-cursors are tripped.
**
** SQLITE_OK is returned if successful, or if an error occurs while
** saving a cursor position, an SQLite error code.
*/
int sqlite3BtreeTripAllCursors(Btree *pBtree, int errCode, int writeOnly){
  BtCursor *p;
  int rc = SQLITE_OK;

  assert( (writeOnly==0 || writeOnly==1) && BTCF_WriteFlag==1 );
  if( pBtree ){
    sqlite3BtreeEnter(pBtree);
    for(p=pBtree->pBt->pCursor; p; p=p->pNext){
      if( writeOnly && (p->curFlags & BTCF_WriteFlag)==0 ){
        if( p->eState==CURSOR_VALID || p->eState==CURSOR_SKIPNEXT ){
          rc = saveCursorPosition(p);
          if( rc!=SQLITE_OK ){
            (void)sqlite3BtreeTripAllCursors(pBtree, rc, 0);
            break;
          }
        }
      }else{
        sqlite3BtreeClearCursor(p);
        p->eState = CURSOR_FAULT;
        p->skipNext = errCode;
      }
      btreeReleaseAllCursorPages(p);
    }
    sqlite3BtreeLeave(pBtree);
  }
  return rc;
}

/*
** Set the pBt->nPage field correctly, according to the current
** state of the database.  Assume pBt->pPage1 is valid.
*/
static void btreeSetNPage(BtShared *pBt, MemPage *pPage1){
  int nPage = get4byte(&pPage1->aData[28]);
  testcase( nPage==0 );
  if( nPage==0 ) sqlite3PagerPagecount(pBt->pPager, &nPage);
  testcase( pBt->nPage!=(u32)nPage );
  pBt->nPage = nPage;
}

/*
** Rollback the transaction in progress.
**
** If tripCode is not SQLITE_OK then cursors will be invalidated (tripped).
** Only write cursors are tripped if writeOnly is true but all cursors are
** tripped if writeOnly is false.  Any attempt to use
** a tripped cursor will result in an error.
**
** This will release the write lock on the database file.  If there
** are no active cursors, it also releases the read lock.
*/
int sqlite3BtreeRollback(Btree *p, int tripCode, int writeOnly){
  int rc;
  BtShared *pBt = p->pBt;
  MemPage *pPage1;

  assert( writeOnly==1 || writeOnly==0 );
  assert( tripCode==SQLITE_ABORT_ROLLBACK || tripCode==SQLITE_OK );
  sqlite3BtreeEnter(p);
  if( tripCode==SQLITE_OK ){
    rc = tripCode = saveAllCursors(pBt, 0, 0);
    if( rc ) writeOnly = 0;
  }else{
    rc = SQLITE_OK;
  }
  if( tripCode ){
    int rc2 = sqlite3BtreeTripAllCursors(p, tripCode, writeOnly);
    assert( rc==SQLITE_OK || (writeOnly==0 && rc2==SQLITE_OK) );
    if( rc2!=SQLITE_OK ) rc = rc2;
  }
  btreeIntegrity(p);

  if( p->inTrans==TRANS_WRITE ){
    int rc2;

    assert( TRANS_WRITE==pBt->inTransaction );
    rc2 = sqlite3PagerRollback(pBt->pPager);
    if( rc2!=SQLITE_OK ){
      rc = rc2;
    }

    /* The rollback may have destroyed the pPage1->aData value.  So
    ** call btreeGetPage() on page 1 again to make
    ** sure pPage1->aData is set correctly. */
    if( btreeGetPage(pBt, 1, &pPage1, 0)==SQLITE_OK ){
      btreeSetNPage(pBt, pPage1);
      releasePageOne(pPage1);
    }
    assert( countValidCursors(pBt, 1)==0 );
    pBt->inTransaction = TRANS_READ;
    btreeClearHasContent(pBt);
  }

  btreeEndTransaction(p);
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** Start a statement subtransaction. The subtransaction can be rolled
** back independently of the main transaction. You must start a transaction 
** before starting a subtransaction. The subtransaction is ended automatically 
** if the main transaction commits or rolls back.
**
** Statement subtransactions are used around individual SQL statements
** that are contained within a BEGIN...COMMIT block.  If a constraint
** error occurs within the statement, the effect of that one statement
** can be rolled back without having to rollback the entire transaction.
**
** A statement sub-transaction is implemented as an anonymous savepoint. The
** value passed as the second parameter is the total number of savepoints,
** including the new anonymous savepoint, open on the B-Tree. i.e. if there
** are no active savepoints and no other statement-transactions open,
** iStatement is 1. This anonymous savepoint can be released or rolled back
** using the sqlite3BtreeSavepoint() function.
*/
int sqlite3BtreeBeginStmt(Btree *p, int iStatement){
  int rc;
  BtShared *pBt = p->pBt;
  sqlite3BtreeEnter(p);
  assert( p->inTrans==TRANS_WRITE );
  assert( (pBt->btsFlags & BTS_READ_ONLY)==0 );
  assert( iStatement>0 );
  assert( iStatement>p->db->nSavepoint );
  assert( pBt->inTransaction==TRANS_WRITE );
  /* At the pager level, a statement transaction is a savepoint with
  ** an index greater than all savepoints created explicitly using
  ** SQL statements. It is illegal to open, release or rollback any
  ** such savepoints while the statement transaction savepoint is active.
  */
  rc = sqlite3PagerOpenSavepoint(pBt->pPager, iStatement);
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** The second argument to this function, op, is always SAVEPOINT_ROLLBACK
** or SAVEPOINT_RELEASE. This function either releases or rolls back the
** savepoint identified by parameter iSavepoint, depending on the value 
** of op.
**
** Normally, iSavepoint is greater than or equal to zero. However, if op is
** SAVEPOINT_ROLLBACK, then iSavepoint may also be -1. In this case the 
** contents of the entire transaction are rolled back. This is different
** from a normal transaction rollback, as no locks are released and the
** transaction remains open.
*/
int sqlite3BtreeSavepoint(Btree *p, int op, int iSavepoint){
  int rc = SQLITE_OK;
  if( p && p->inTrans==TRANS_WRITE ){
    BtShared *pBt = p->pBt;
    assert( op==SAVEPOINT_RELEASE || op==SAVEPOINT_ROLLBACK );
    assert( iSavepoint>=0 || (iSavepoint==-1 && op==SAVEPOINT_ROLLBACK) );
    sqlite3BtreeEnter(p);
    if( op==SAVEPOINT_ROLLBACK ){
      rc = saveAllCursors(pBt, 0, 0);
    }
    if( rc==SQLITE_OK ){
      rc = sqlite3PagerSavepoint(pBt->pPager, op, iSavepoint);
    }
    if( rc==SQLITE_OK ){
      if( iSavepoint<0 && (pBt->btsFlags & BTS_INITIALLY_EMPTY)!=0 ){
        pBt->nPage = 0;
      }
      rc = newDatabase(pBt);
      btreeSetNPage(pBt, pBt->pPage1);

      /* pBt->nPage might be zero if the database was corrupt when 
      ** the transaction was started. Otherwise, it must be at least 1.  */
      assert( CORRUPT_DB || pBt->nPage>0 );
    }
    sqlite3BtreeLeave(p);
  }
  return rc;
}

/*
** Create a new cursor for the BTree whose root is on the page
** iTable. If a read-only cursor is requested, it is assumed that
** the caller already has at least a read-only transaction open
** on the database already. If a write-cursor is requested, then
** the caller is assumed to have an open write transaction.
**
** If the BTREE_WRCSR bit of wrFlag is clear, then the cursor can only
** be used for reading.  If the BTREE_WRCSR bit is set, then the cursor
** can be used for reading or for writing if other conditions for writing
** are also met.  These are the conditions that must be met in order
** for writing to be allowed:
**
** 1:  The cursor must have been opened with wrFlag containing BTREE_WRCSR
**
** 2:  Other database connections that share the same pager cache
**     but which are not in the READ_UNCOMMITTED state may not have
**     cursors open with wrFlag==0 on the same table.  Otherwise
**     the changes made by this write cursor would be visible to
**     the read cursors in the other database connection.
**
** 3:  The database must be writable (not on read-only media)
**
** 4:  There must be an active transaction.
**
** The BTREE_FORDELETE bit of wrFlag may optionally be set if BTREE_WRCSR
** is set.  If FORDELETE is set, that is a hint to the implementation that
** this cursor will only be used to seek to and delete entries of an index
** as part of a larger DELETE statement.  The FORDELETE hint is not used by
** this implementation.  But in a hypothetical alternative storage engine 
** in which index entries are automatically deleted when corresponding table
** rows are deleted, the FORDELETE flag is a hint that all SEEK and DELETE
** operations on this cursor can be no-ops and all READ operations can 
** return a null row (2-bytes: 0x01 0x00).
**
** No checking is done to make sure that page iTable really is the
** root page of a b-tree.  If it is not, then the cursor acquired
** will not work correctly.
**
** It is assumed that the sqlite3BtreeCursorZero() has been called
** on pCur to initialize the memory space prior to invoking this routine.
*/
static int btreeCursor(
  Btree *p,                              /* The btree */
  Pgno iTable,                           /* Root page of table to open */
  int wrFlag,                            /* 1 to write. 0 read-only */
  struct KeyInfo *pKeyInfo,              /* First arg to comparison function */
  BtCursor *pCur                         /* Space for new cursor */
){
  BtShared *pBt = p->pBt;                /* Shared b-tree handle */
  BtCursor *pX;                          /* Looping over other all cursors */

  assert( sqlite3BtreeHoldsMutex(p) );
  assert( wrFlag==0 
       || wrFlag==BTREE_WRCSR 
       || wrFlag==(BTREE_WRCSR|BTREE_FORDELETE) 
  );

  /* The following assert statements verify that if this is a sharable 
  ** b-tree database, the connection is holding the required table locks, 
  ** and that no other connection has any open cursor that conflicts with 
  ** this lock.  The iTable<1 term disables the check for corrupt schemas. */
  assert( hasSharedCacheTableLock(p, iTable, pKeyInfo!=0, (wrFlag?2:1))
          || iTable<1 );
  assert( wrFlag==0 || !hasReadConflicts(p, iTable) );

  /* Assert that the caller has opened the required transaction. */
  assert( p->inTrans>TRANS_NONE );
  assert( wrFlag==0 || p->inTrans==TRANS_WRITE );
  assert( pBt->pPage1 && pBt->pPage1->aData );
  assert( wrFlag==0 || (pBt->btsFlags & BTS_READ_ONLY)==0 );

  if( iTable<=1 ){
    if( iTable<1 ){
      return SQLITE_CORRUPT_BKPT;
    }else if( btreePagecount(pBt)==0 ){
      assert( wrFlag==0 );
      iTable = 0;
    }
  }

  /* Now that no other errors can occur, finish filling in the BtCursor
  ** variables and link the cursor into the BtShared list.  */
  pCur->pgnoRoot = iTable;
  pCur->iPage = -1;
  pCur->pKeyInfo = pKeyInfo;
  pCur->pBtree = p;
  pCur->pBt = pBt;
  pCur->curFlags = 0;
  /* If there are two or more cursors on the same btree, then all such
  ** cursors *must* have the BTCF_Multiple flag set. */
  for(pX=pBt->pCursor; pX; pX=pX->pNext){
    if( pX->pgnoRoot==iTable ){
      pX->curFlags |= BTCF_Multiple;
      pCur->curFlags = BTCF_Multiple;
    }
  }
  pCur->eState = CURSOR_INVALID;
  pCur->pNext = pBt->pCursor;
  pBt->pCursor = pCur;
  if( wrFlag ){
    pCur->curFlags |= BTCF_WriteFlag;
    pCur->curPagerFlags = 0;
    if( pBt->pTmpSpace==0 ) return allocateTempSpace(pBt);
  }else{
    pCur->curPagerFlags = PAGER_GET_READONLY;
  }
  return SQLITE_OK;
}
static int btreeCursorWithLock(
  Btree *p,                              /* The btree */
  Pgno iTable,                           /* Root page of table to open */
  int wrFlag,                            /* 1 to write. 0 read-only */
  struct KeyInfo *pKeyInfo,              /* First arg to comparison function */
  BtCursor *pCur                         /* Space for new cursor */
){
  int rc;
  sqlite3BtreeEnter(p);
  rc = btreeCursor(p, iTable, wrFlag, pKeyInfo, pCur);
  sqlite3BtreeLeave(p);
  return rc;
}
int sqlite3BtreeCursor(
  Btree *p,                                   /* The btree */
  Pgno iTable,                                /* Root page of table to open */
  int wrFlag,                                 /* 1 to write. 0 read-only */
  struct KeyInfo *pKeyInfo,                   /* First arg to xCompare() */
  BtCursor *pCur                              /* Write new cursor here */
){
  if( p->sharable ){
    return btreeCursorWithLock(p, iTable, wrFlag, pKeyInfo, pCur);
  }else{
    return btreeCursor(p, iTable, wrFlag, pKeyInfo, pCur);
  }
}

/*
** Return the size of a BtCursor object in bytes.
**
** This interfaces is needed so that users of cursors can preallocate
** sufficient storage to hold a cursor.  The BtCursor object is opaque
** to users so they cannot do the sizeof() themselves - they must call
** this routine.
*/
int sqlite3BtreeCursorSize(void){
  return ROUND8(sizeof(BtCursor));
}

/*
** Initialize memory that will be converted into a BtCursor object.
**
** The simple approach here would be to memset() the entire object
** to zero.  But it turns out that the apPage[] and aiIdx[] arrays
** do not need to be zeroed and they are large, so we can save a lot
** of run-time by skipping the initialization of those elements.
*/
void sqlite3BtreeCursorZero(BtCursor *p){
  memset(p, 0, offsetof(BtCursor, BTCURSOR_FIRST_UNINIT));
}

/*
** Close a cursor.  The read lock on the database file is released
** when the last cursor is closed.
*/
int sqlite3BtreeCloseCursor(BtCursor *pCur){
  Btree *pBtree = pCur->pBtree;
  if( pBtree ){
    BtShared *pBt = pCur->pBt;
    sqlite3BtreeEnter(pBtree);
    assert( pBt->pCursor!=0 );
    if( pBt->pCursor==pCur ){
      pBt->pCursor = pCur->pNext;
    }else{
      BtCursor *pPrev = pBt->pCursor;
      do{
        if( pPrev->pNext==pCur ){
          pPrev->pNext = pCur->pNext;
          break;
        }
        pPrev = pPrev->pNext;
      }while( ALWAYS(pPrev) );
    }
    btreeReleaseAllCursorPages(pCur);
    unlockBtreeIfUnused(pBt);
    sqlite3_free(pCur->aOverflow);
    sqlite3_free(pCur->pKey);
    if( (pBt->openFlags & BTREE_SINGLE) && pBt->pCursor==0 ){
      /* Since the BtShared is not sharable, there is no need to
      ** worry about the missing sqlite3BtreeLeave() call here.  */
      assert( pBtree->sharable==0 );
      sqlite3BtreeClose(pBtree);
    }else{
      sqlite3BtreeLeave(pBtree);
    }
    pCur->pBtree = 0;
  }
  return SQLITE_OK;
}

/*
** Make sure the BtCursor* given in the argument has a valid
** BtCursor.info structure.  If it is not already valid, call
** btreeParseCell() to fill it in.
**
** BtCursor.info is a cache of the information in the current cell.
** Using this cache reduces the number of calls to btreeParseCell().
*/
#ifndef NDEBUG
  static int cellInfoEqual(CellInfo *a, CellInfo *b){
    if( a->nKey!=b->nKey ) return 0;
    if( a->pPayload!=b->pPayload ) return 0;
    if( a->nPayload!=b->nPayload ) return 0;
    if( a->nLocal!=b->nLocal ) return 0;
    if( a->nSize!=b->nSize ) return 0;
    return 1;
  }
  static void assertCellInfo(BtCursor *pCur){
    CellInfo info;
    memset(&info, 0, sizeof(info));
    btreeParseCell(pCur->pPage, pCur->ix, &info);
    assert( CORRUPT_DB || cellInfoEqual(&info, &pCur->info) );
  }
#else
  #define assertCellInfo(x)
#endif
static SQLITE_NOINLINE void getCellInfo(BtCursor *pCur){
  if( pCur->info.nSize==0 ){
    pCur->curFlags |= BTCF_ValidNKey;
    btreeParseCell(pCur->pPage,pCur->ix,&pCur->info);
  }else{
    assertCellInfo(pCur);
  }
}

#ifndef NDEBUG  /* The next routine used only within assert() statements */
/*
** Return true if the given BtCursor is valid.  A valid cursor is one
** that is currently pointing to a row in a (non-empty) table.
** This is a verification routine is used only within assert() statements.
*/
int sqlite3BtreeCursorIsValid(BtCursor *pCur){
  return pCur && pCur->eState==CURSOR_VALID;
}
#endif /* NDEBUG */
int sqlite3BtreeCursorIsValidNN(BtCursor *pCur){
  assert( pCur!=0 );
  return pCur->eState==CURSOR_VALID;
}

/*
** Return the value of the integer key or "rowid" for a table btree.
** This routine is only valid for a cursor that is pointing into a
** ordinary table btree.  If the cursor points to an index btree or
** is invalid, the result of this routine is undefined.
*/
i64 sqlite3BtreeIntegerKey(BtCursor *pCur){
  assert( cursorHoldsMutex(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  assert( pCur->curIntKey );
  getCellInfo(pCur);
  return pCur->info.nKey;
}

/*
** Pin or unpin a cursor.
*/
void sqlite3BtreeCursorPin(BtCursor *pCur){
  assert( (pCur->curFlags & BTCF_Pinned)==0 );
  pCur->curFlags |= BTCF_Pinned;
}
void sqlite3BtreeCursorUnpin(BtCursor *pCur){
  assert( (pCur->curFlags & BTCF_Pinned)!=0 );
  pCur->curFlags &= ~BTCF_Pinned;
}

#ifdef SQLITE_ENABLE_OFFSET_SQL_FUNC
/*
** Return the offset into the database file for the start of the
** payload to which the cursor is pointing.
*/
i64 sqlite3BtreeOffset(BtCursor *pCur){
  assert( cursorHoldsMutex(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  getCellInfo(pCur);
  return (i64)pCur->pBt->pageSize*((i64)pCur->pPage->pgno - 1) +
         (i64)(pCur->info.pPayload - pCur->pPage->aData);
}
#endif /* SQLITE_ENABLE_OFFSET_SQL_FUNC */

/*
** Return the number of bytes of payload for the entry that pCur is
** currently pointing to.  For table btrees, this will be the amount
** of data.  For index btrees, this will be the size of the key.
**
** The caller must guarantee that the cursor is pointing to a non-NULL
** valid entry.  In other words, the calling procedure must guarantee
** that the cursor has Cursor.eState==CURSOR_VALID.
*/
u32 sqlite3BtreePayloadSize(BtCursor *pCur){
  assert( cursorHoldsMutex(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  getCellInfo(pCur);
  return pCur->info.nPayload;
}

/*
** Return an upper bound on the size of any record for the table
** that the cursor is pointing into.
**
** This is an optimization.  Everything will still work if this
** routine always returns 2147483647 (which is the largest record
** that SQLite can handle) or more.  But returning a smaller value might
** prevent large memory allocations when trying to interpret a
** corrupt datrabase.
**
** The current implementation merely returns the size of the underlying
** database file.
*/
sqlite3_int64 sqlite3BtreeMaxRecordSize(BtCursor *pCur){
  assert( cursorHoldsMutex(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  return pCur->pBt->pageSize * (sqlite3_int64)pCur->pBt->nPage;
}

/*
** Given the page number of an overflow page in the database (parameter
** ovfl), this function finds the page number of the next page in the 
** linked list of overflow pages. If possible, it uses the auto-vacuum
** pointer-map data instead of reading the content of page ovfl to do so. 
**
** If an error occurs an SQLite error code is returned. Otherwise:
**
** The page number of the next overflow page in the linked list is 
** written to *pPgnoNext. If page ovfl is the last page in its linked 
** list, *pPgnoNext is set to zero. 
**
** If ppPage is not NULL, and a reference to the MemPage object corresponding
** to page number pOvfl was obtained, then *ppPage is set to point to that
** reference. It is the responsibility of the caller to call releasePage()
** on *ppPage to free the reference. In no reference was obtained (because
** the pointer-map was used to obtain the value for *pPgnoNext), then
** *ppPage is set to zero.
*/
static int getOverflowPage(
  BtShared *pBt,               /* The database file */
  Pgno ovfl,                   /* Current overflow page number */
  MemPage **ppPage,            /* OUT: MemPage handle (may be NULL) */
  Pgno *pPgnoNext              /* OUT: Next overflow page number */
){
  Pgno next = 0;
  MemPage *pPage = 0;
  int rc = SQLITE_OK;

  assert( sqlite3_mutex_held(pBt->mutex) );
  assert(pPgnoNext);

#ifndef SQLITE_OMIT_AUTOVACUUM
  /* Try to find the next page in the overflow list using the
  ** autovacuum pointer-map pages. Guess that the next page in 
  ** the overflow list is page number (ovfl+1). If that guess turns 
  ** out to be wrong, fall back to loading the data of page 
  ** number ovfl to determine the next page number.
  */
  if( pBt->autoVacuum ){
    Pgno pgno;
    Pgno iGuess = ovfl+1;
    u8 eType;

    while( PTRMAP_ISPAGE(pBt, iGuess) || iGuess==PENDING_BYTE_PAGE(pBt) ){
      iGuess++;
    }

    if( iGuess<=btreePagecount(pBt) ){
      rc = ptrmapGet(pBt, iGuess, &eType, &pgno);
      if( rc==SQLITE_OK && eType==PTRMAP_OVERFLOW2 && pgno==ovfl ){
        next = iGuess;
        rc = SQLITE_DONE;
      }
    }
  }
#endif

  assert( next==0 || rc==SQLITE_DONE );
  if( rc==SQLITE_OK ){
    rc = btreeGetPage(pBt, ovfl, &pPage, (ppPage==0) ? PAGER_GET_READONLY : 0);
    assert( rc==SQLITE_OK || pPage==0 );
    if( rc==SQLITE_OK ){
      next = get4byte(pPage->aData);
    }
  }

  *pPgnoNext = next;
  if( ppPage ){
    *ppPage = pPage;
  }else{
    releasePage(pPage);
  }
  return (rc==SQLITE_DONE ? SQLITE_OK : rc);
}

/*
** Copy data from a buffer to a page, or from a page to a buffer.
**
** pPayload is a pointer to data stored on database page pDbPage.
** If argument eOp is false, then nByte bytes of data are copied
** from pPayload to the buffer pointed at by pBuf. If eOp is true,
** then sqlite3PagerWrite() is called on pDbPage and nByte bytes
** of data are copied from the buffer pBuf to pPayload.
**
** SQLITE_OK is returned on success, otherwise an error code.
*/
static int copyPayload(
  void *pPayload,           /* Pointer to page data */
  void *pBuf,               /* Pointer to buffer */
  int nByte,                /* Number of bytes to copy */
  int eOp,                  /* 0 -> copy from page, 1 -> copy to page */
  DbPage *pDbPage           /* Page containing pPayload */
){
  if( eOp ){
    /* Copy data from buffer to page (a write operation) */
    int rc = sqlite3PagerWrite(pDbPage);
    if( rc!=SQLITE_OK ){
      return rc;
    }
    memcpy(pPayload, pBuf, nByte);
  }else{
    /* Copy data from page to buffer (a read operation) */
    memcpy(pBuf, pPayload, nByte);
  }
  return SQLITE_OK;
}

/*
** This function is used to read or overwrite payload information
** for the entry that the pCur cursor is pointing to. The eOp
** argument is interpreted as follows:
**
**   0: The operation is a read. Populate the overflow cache.
**   1: The operation is a write. Populate the overflow cache.
**
** A total of "amt" bytes are read or written beginning at "offset".
** Data is read to or from the buffer pBuf.
**
** The content being read or written might appear on the main page
** or be scattered out on multiple overflow pages.
**
** If the current cursor entry uses one or more overflow pages
** this function may allocate space for and lazily populate
** the overflow page-list cache array (BtCursor.aOverflow). 
** Subsequent calls use this cache to make seeking to the supplied offset 
** more efficient.
**
** Once an overflow page-list cache has been allocated, it must be
** invalidated if some other cursor writes to the same table, or if
** the cursor is moved to a different row. Additionally, in auto-vacuum
** mode, the following events may invalidate an overflow page-list cache.
**
**   * An incremental vacuum,
**   * A commit in auto_vacuum="full" mode,
**   * Creating a table (may require moving an overflow page).
*/
static int accessPayload(
  BtCursor *pCur,      /* Cursor pointing to entry to read from */
  u32 offset,          /* Begin reading this far into payload */
  u32 amt,             /* Read this many bytes */
  unsigned char *pBuf, /* Write the bytes into this buffer */ 
  int eOp              /* zero to read. non-zero to write. */
){
  unsigned char *aPayload;
  int rc = SQLITE_OK;
  int iIdx = 0;
  MemPage *pPage = pCur->pPage;               /* Btree page of current entry */
  BtShared *pBt = pCur->pBt;                  /* Btree this cursor belongs to */
#ifdef SQLITE_DIRECT_OVERFLOW_READ
  unsigned char * const pBufStart = pBuf;     /* Start of original out buffer */
#endif

  assert( pPage );
  assert( eOp==0 || eOp==1 );
  assert( pCur->eState==CURSOR_VALID );
  if( pCur->ix>=pPage->nCell ){
    return SQLITE_CORRUPT_PAGE(pPage);
  }
  assert( cursorHoldsMutex(pCur) );

  getCellInfo(pCur);
  aPayload = pCur->info.pPayload;
  assert( offset+amt <= pCur->info.nPayload );

  assert( aPayload > pPage->aData );
  if( (uptr)(aPayload - pPage->aData) > (pBt->usableSize - pCur->info.nLocal) ){
    /* Trying to read or write past the end of the data is an error.  The
    ** conditional above is really:
    **    &aPayload[pCur->info.nLocal] > &pPage->aData[pBt->usableSize]
    ** but is recast into its current form to avoid integer overflow problems
    */
    return SQLITE_CORRUPT_PAGE(pPage);
  }

  /* Check if data must be read/written to/from the btree page itself. */
  if( offset<pCur->info.nLocal ){
    int a = amt;
    if( a+offset>pCur->info.nLocal ){
      a = pCur->info.nLocal - offset;
    }
    rc = copyPayload(&aPayload[offset], pBuf, a, eOp, pPage->pDbPage);
    offset = 0;
    pBuf += a;
    amt -= a;
  }else{
    offset -= pCur->info.nLocal;
  }


  if( rc==SQLITE_OK && amt>0 ){
    const u32 ovflSize = pBt->usableSize - 4;  /* Bytes content per ovfl page */
    Pgno nextPage;

    nextPage = get4byte(&aPayload[pCur->info.nLocal]);
 
    /* If the BtCursor.aOverflow[] has not been allocated, allocate it now.
    **
    ** The aOverflow[] array is sized at one entry for each overflow page
    ** in the overflow chain. The page number of the first overflow page is
    ** stored in aOverflow[0], etc. A value of 0 in the aOverflow[] array
    ** means "not yet known" (the cache is lazily populated).
    */
    if( (pCur->curFlags & BTCF_ValidOvfl)==0 ){
      int nOvfl = (pCur->info.nPayload-pCur->info.nLocal+ovflSize-1)/ovflSize;
      if( pCur->aOverflow==0
       || nOvfl*(int)sizeof(Pgno) > sqlite3MallocSize(pCur->aOverflow)
      ){
        Pgno *aNew = (Pgno*)sqlite3Realloc(
            pCur->aOverflow, nOvfl*2*sizeof(Pgno)
        );
        if( aNew==0 ){
          return SQLITE_NOMEM_BKPT;
        }else{
          pCur->aOverflow = aNew;
        }
      }
      memset(pCur->aOverflow, 0, nOvfl*sizeof(Pgno));
      pCur->curFlags |= BTCF_ValidOvfl;
    }else{
      /* If the overflow page-list cache has been allocated and the
      ** entry for the first required overflow page is valid, skip
      ** directly to it.
      */
      if( pCur->aOverflow[offset/ovflSize] ){
        iIdx = (offset/ovflSize);
        nextPage = pCur->aOverflow[iIdx];
        offset = (offset%ovflSize);
      }
    }

    assert( rc==SQLITE_OK && amt>0 );
    while( nextPage ){
      /* If required, populate the overflow page-list cache. */
      if( nextPage > pBt->nPage ) return SQLITE_CORRUPT_BKPT;
      assert( pCur->aOverflow[iIdx]==0
              || pCur->aOverflow[iIdx]==nextPage
              || CORRUPT_DB );
      pCur->aOverflow[iIdx] = nextPage;

      if( offset>=ovflSize ){
        /* The only reason to read this page is to obtain the page
        ** number for the next page in the overflow chain. The page
        ** data is not required. So first try to lookup the overflow
        ** page-list cache, if any, then fall back to the getOverflowPage()
        ** function.
        */
        assert( pCur->curFlags & BTCF_ValidOvfl );
        assert( pCur->pBtree->db==pBt->db );
        if( pCur->aOverflow[iIdx+1] ){
          nextPage = pCur->aOverflow[iIdx+1];
        }else{
          rc = getOverflowPage(pBt, nextPage, 0, &nextPage);
        }
        offset -= ovflSize;
      }else{
        /* Need to read this page properly. It contains some of the
        ** range of data that is being read (eOp==0) or written (eOp!=0).
        */
        int a = amt;
        if( a + offset > ovflSize ){
          a = ovflSize - offset;
        }

#ifdef SQLITE_DIRECT_OVERFLOW_READ
        /* If all the following are true:
        **
        **   1) this is a read operation, and 
        **   2) data is required from the start of this overflow page, and
        **   3) there are no dirty pages in the page-cache
        **   4) the database is file-backed, and
        **   5) the page is not in the WAL file
        **   6) at least 4 bytes have already been read into the output buffer 
        **
        ** then data can be read directly from the database file into the
        ** output buffer, bypassing the page-cache altogether. This speeds
        ** up loading large records that span many overflow pages.
        */
        if( eOp==0                                             /* (1) */
         && offset==0                                          /* (2) */
         && sqlite3PagerDirectReadOk(pBt->pPager, nextPage)    /* (3,4,5) */
         && &pBuf[-4]>=pBufStart                               /* (6) */
        ){
          sqlite3_file *fd = sqlite3PagerFile(pBt->pPager);
          u8 aSave[4];
          u8 *aWrite = &pBuf[-4];
          assert( aWrite>=pBufStart );                         /* due to (6) */
          memcpy(aSave, aWrite, 4);
          rc = sqlite3OsRead(fd, aWrite, a+4, (i64)pBt->pageSize*(nextPage-1));
          if( rc && nextPage>pBt->nPage ) rc = SQLITE_CORRUPT_BKPT;
          nextPage = get4byte(aWrite);
          memcpy(aWrite, aSave, 4);
        }else
#endif

        {
          DbPage *pDbPage;
          rc = sqlite3PagerGet(pBt->pPager, nextPage, &pDbPage,
              (eOp==0 ? PAGER_GET_READONLY : 0)
          );
          if( rc==SQLITE_OK ){
            aPayload = sqlite3PagerGetData(pDbPage);
            nextPage = get4byte(aPayload);
            rc = copyPayload(&aPayload[offset+4], pBuf, a, eOp, pDbPage);
            sqlite3PagerUnref(pDbPage);
            offset = 0;
          }
        }
        amt -= a;
        if( amt==0 ) return rc;
        pBuf += a;
      }
      if( rc ) break;
      iIdx++;
    }
  }

  if( rc==SQLITE_OK && amt>0 ){
    /* Overflow chain ends prematurely */
    return SQLITE_CORRUPT_PAGE(pPage);
  }
  return rc;
}

/*
** Read part of the payload for the row at which that cursor pCur is currently
** pointing.  "amt" bytes will be transferred into pBuf[].  The transfer
** begins at "offset".
**
** pCur can be pointing to either a table or an index b-tree.
** If pointing to a table btree, then the content section is read.  If
** pCur is pointing to an index b-tree then the key section is read.
**
** For sqlite3BtreePayload(), the caller must ensure that pCur is pointing
** to a valid row in the table.  For sqlite3BtreePayloadChecked(), the
** cursor might be invalid or might need to be restored before being read.
**
** Return SQLITE_OK on success or an error code if anything goes
** wrong.  An error is returned if "offset+amt" is larger than
** the available payload.
*/
int sqlite3BtreePayload(BtCursor *pCur, u32 offset, u32 amt, void *pBuf){
  assert( cursorHoldsMutex(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  assert( pCur->iPage>=0 && pCur->pPage );
  return accessPayload(pCur, offset, amt, (unsigned char*)pBuf, 0);
}

/*
** This variant of sqlite3BtreePayload() works even if the cursor has not
** in the CURSOR_VALID state.  It is only used by the sqlite3_blob_read()
** interface.
*/
#ifndef SQLITE_OMIT_INCRBLOB
static SQLITE_NOINLINE int accessPayloadChecked(
  BtCursor *pCur,
  u32 offset,
  u32 amt,
  void *pBuf
){
  int rc;
  if ( pCur->eState==CURSOR_INVALID ){
    return SQLITE_ABORT;
  }
  assert( cursorOwnsBtShared(pCur) );
  rc = btreeRestoreCursorPosition(pCur);
  return rc ? rc : accessPayload(pCur, offset, amt, pBuf, 0);
}
int sqlite3BtreePayloadChecked(BtCursor *pCur, u32 offset, u32 amt, void *pBuf){
  if( pCur->eState==CURSOR_VALID ){
    assert( cursorOwnsBtShared(pCur) );
    return accessPayload(pCur, offset, amt, pBuf, 0);
  }else{
    return accessPayloadChecked(pCur, offset, amt, pBuf);
  }
}
#endif /* SQLITE_OMIT_INCRBLOB */

/*
** Return a pointer to payload information from the entry that the 
** pCur cursor is pointing to.  The pointer is to the beginning of
** the key if index btrees (pPage->intKey==0) and is the data for
** table btrees (pPage->intKey==1). The number of bytes of available
** key/data is written into *pAmt.  If *pAmt==0, then the value
** returned will not be a valid pointer.
**
** This routine is an optimization.  It is common for the entire key
** and data to fit on the local page and for there to be no overflow
** pages.  When that is so, this routine can be used to access the
** key and data without making a copy.  If the key and/or data spills
** onto overflow pages, then accessPayload() must be used to reassemble
** the key/data and copy it into a preallocated buffer.
**
** The pointer returned by this routine looks directly into the cached
** page of the database.  The data might change or move the next time
** any btree routine is called.
*/
static const void *fetchPayload(
  BtCursor *pCur,      /* Cursor pointing to entry to read from */
  u32 *pAmt            /* Write the number of available bytes here */
){
  int amt;
  assert( pCur!=0 && pCur->iPage>=0 && pCur->pPage);
  assert( pCur->eState==CURSOR_VALID );
  assert( sqlite3_mutex_held(pCur->pBtree->db->mutex) );
  assert( cursorOwnsBtShared(pCur) );
  assert( pCur->ix<pCur->pPage->nCell || CORRUPT_DB );
  assert( pCur->info.nSize>0 );
  assert( pCur->info.pPayload>pCur->pPage->aData || CORRUPT_DB );
  assert( pCur->info.pPayload<pCur->pPage->aDataEnd ||CORRUPT_DB);
  amt = pCur->info.nLocal;
  if( amt>(int)(pCur->pPage->aDataEnd - pCur->info.pPayload) ){
    /* There is too little space on the page for the expected amount
    ** of local content. Database must be corrupt. */
    assert( CORRUPT_DB );
    amt = MAX(0, (int)(pCur->pPage->aDataEnd - pCur->info.pPayload));
  }
  *pAmt = (u32)amt;
  return (void*)pCur->info.pPayload;
}


/*
** For the entry that cursor pCur is point to, return as
** many bytes of the key or data as are available on the local
** b-tree page.  Write the number of available bytes into *pAmt.
**
** The pointer returned is ephemeral.  The key/data may move
** or be destroyed on the next call to any Btree routine,
** including calls from other threads against the same cache.
** Hence, a mutex on the BtShared should be held prior to calling
** this routine.
**
** These routines is used to get quick access to key and data
** in the common case where no overflow pages are used.
*/
const void *sqlite3BtreePayloadFetch(BtCursor *pCur, u32 *pAmt){
  return fetchPayload(pCur, pAmt);
}


/*
** Move the cursor down to a new child page.  The newPgno argument is the
** page number of the child page to move to.
**
** This function returns SQLITE_CORRUPT if the page-header flags field of
** the new child page does not match the flags field of the parent (i.e.
** if an intkey page appears to be the parent of a non-intkey page, or
** vice-versa).
*/
static int moveToChild(BtCursor *pCur, u32 newPgno){
  assert( cursorOwnsBtShared(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  assert( pCur->iPage<BTCURSOR_MAX_DEPTH );
  assert( pCur->iPage>=0 );
  if( pCur->iPage>=(BTCURSOR_MAX_DEPTH-1) ){
    return SQLITE_CORRUPT_BKPT;
  }
  pCur->info.nSize = 0;
  pCur->curFlags &= ~(BTCF_ValidNKey|BTCF_ValidOvfl);
  pCur->aiIdx[pCur->iPage] = pCur->ix;
  pCur->apPage[pCur->iPage] = pCur->pPage;
  pCur->ix = 0;
  pCur->iPage++;
  return getAndInitPage(pCur->pBt, newPgno, &pCur->pPage, pCur,
                        pCur->curPagerFlags);
}

#ifdef SQLITE_DEBUG
/*
** Page pParent is an internal (non-leaf) tree page. This function 
** asserts that page number iChild is the left-child if the iIdx'th
** cell in page pParent. Or, if iIdx is equal to the total number of
** cells in pParent, that page number iChild is the right-child of
** the page.
*/
static void assertParentIndex(MemPage *pParent, int iIdx, Pgno iChild){
  if( CORRUPT_DB ) return;  /* The conditions tested below might not be true
                            ** in a corrupt database */
  assert( iIdx<=pParent->nCell );
  if( iIdx==pParent->nCell ){
    assert( get4byte(&pParent->aData[pParent->hdrOffset+8])==iChild );
  }else{
    assert( get4byte(findCell(pParent, iIdx))==iChild );
  }
}
#else
#  define assertParentIndex(x,y,z) 
#endif

/*
** Move the cursor up to the parent page.
**
** pCur->idx is set to the cell index that contains the pointer
** to the page we are coming from.  If we are coming from the
** right-most child page then pCur->idx is set to one more than
** the largest cell index.
*/
static void moveToParent(BtCursor *pCur){
  MemPage *pLeaf;
  assert( cursorOwnsBtShared(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  assert( pCur->iPage>0 );
  assert( pCur->pPage );
  assertParentIndex(
    pCur->apPage[pCur->iPage-1], 
    pCur->aiIdx[pCur->iPage-1], 
    pCur->pPage->pgno
  );
  testcase( pCur->aiIdx[pCur->iPage-1] > pCur->apPage[pCur->iPage-1]->nCell );
  pCur->info.nSize = 0;
  pCur->curFlags &= ~(BTCF_ValidNKey|BTCF_ValidOvfl);
  pCur->ix = pCur->aiIdx[pCur->iPage-1];
  pLeaf = pCur->pPage;
  pCur->pPage = pCur->apPage[--pCur->iPage];
  releasePageNotNull(pLeaf);
}

/*
** 将游标移动到其B树结构的根页面。
**
** 如果表存在虚拟根页面，则将游标移至该虚拟根页面而非实际根页面。当实际根页面不包含单元格且仅有单个子页面时，
** 该表具有虚拟根页面。这种情况仅可能发生在根页面为第1页的表中。
**
** 若B树结构为空，则将游标状态设为CURSOR_INVALID，本例程返回SQLITE_EMPTY。否则，
** 游标将被设置为指向根页面（或虚拟根页面）上的第一个单元格，并将游标状态设为CURSOR_VALID。
**
** 若本函数成功返回，可以假定页头标志表明该[虚拟]根页面是预期的B树页面类型
** （即当打开游标时，若调用者未指定KeyInfo结构，则标志字节应为0x05或0x0D，表示表B树；
** 若调用者指定了KeyInfo结构，则标志字节应为0x02或0x0A，表示索引B树）。
*/
static int moveToRoot(BtCursor *pCur){
  MemPage *pRoot;
  int rc = SQLITE_OK;

  assert( cursorOwnsBtShared(pCur) );
  assert( CURSOR_INVALID < CURSOR_REQUIRESEEK );
  assert( CURSOR_VALID   < CURSOR_REQUIRESEEK );
  assert( CURSOR_FAULT   > CURSOR_REQUIRESEEK );
  assert( pCur->eState < CURSOR_REQUIRESEEK || pCur->iPage<0 );
  assert( pCur->pgnoRoot>0 || pCur->iPage<0 );

  if( pCur->iPage>=0 ){ // 如果游标已经查询过数据的，那释放除根页外的其它父页
    if( pCur->iPage ){
      releasePageNotNull(pCur->pPage);
      while( --pCur->iPage ){
        releasePageNotNull(pCur->apPage[pCur->iPage]);
      }
      pRoot = pCur->pPage = pCur->apPage[0];
      goto skip_init;
    }
  }else if( pCur->pgnoRoot==0 ){
    pCur->eState = CURSOR_INVALID;
    return SQLITE_EMPTY;
  }else{
    assert( pCur->iPage==(-1) );
    if( pCur->eState>=CURSOR_REQUIRESEEK ){
      if( pCur->eState==CURSOR_FAULT ){
        assert( pCur->skipNext!=SQLITE_OK );
        return pCur->skipNext;
      }
      sqlite3BtreeClearCursor(pCur);
    }
    rc = getAndInitPage(pCur->pBt, pCur->pgnoRoot, &pCur->pPage,
                        0, pCur->curPagerFlags);
    if( rc!=SQLITE_OK ){
      pCur->eState = CURSOR_INVALID;
      return rc;
    }
    pCur->iPage = 0;
    pCur->curIntKey = pCur->pPage->intKey;
  }
  pRoot = pCur->pPage;
  assert( pRoot->pgno==pCur->pgnoRoot || CORRUPT_DB );

  /* If pCur->pKeyInfo is not NULL, then the caller that opened this cursor
  ** expected to open it on an index b-tree. Otherwise, if pKeyInfo is
  ** NULL, the caller expects a table b-tree. If this is not the case,
  ** return an SQLITE_CORRUPT error. 
  **
  ** Earlier versions of SQLite assumed that this test could not fail
  ** if the root page was already loaded when this function was called (i.e.
  ** if pCur->iPage>=0). But this is not so if the database is corrupted 
  ** in such a way that page pRoot is linked into a second b-tree table 
  ** (or the freelist).  */
  assert( pRoot->intKey==1 || pRoot->intKey==0 );
  if( pRoot->isInit==0 || (pCur->pKeyInfo==0)!=pRoot->intKey ){
    return SQLITE_CORRUPT_PAGE(pCur->pPage);
  }

skip_init:  
  pCur->ix = 0;
  pCur->info.nSize = 0;
  pCur->curFlags &= ~(BTCF_AtLast|BTCF_ValidNKey|BTCF_ValidOvfl);

  if( pRoot->nCell>0 ){
    pCur->eState = CURSOR_VALID;
  }else if( !pRoot->leaf ){
    Pgno subpage;
    if( pRoot->pgno!=1 ) return SQLITE_CORRUPT_BKPT;
    subpage = get4byte(&pRoot->aData[pRoot->hdrOffset+8]);
    pCur->eState = CURSOR_VALID;
    rc = moveToChild(pCur, subpage);
  }else{
    pCur->eState = CURSOR_INVALID;
    rc = SQLITE_EMPTY;
  }
  return rc;
}

/*
** Move the cursor down to the left-most leaf entry beneath the
** entry to which it is currently pointing.
**
** The left-most leaf is the one with the smallest key - the first
** in ascending order.
*/
static int moveToLeftmost(BtCursor *pCur){
  Pgno pgno;
  int rc = SQLITE_OK;
  MemPage *pPage;

  assert( cursorOwnsBtShared(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  while( rc==SQLITE_OK && !(pPage = pCur->pPage)->leaf ){
    assert( pCur->ix<pPage->nCell );
    pgno = get4byte(findCell(pPage, pCur->ix));
    rc = moveToChild(pCur, pgno);
  }
  return rc;
}

/*
** Move the cursor down to the right-most leaf entry beneath the
** page to which it is currently pointing.  Notice the difference
** between moveToLeftmost() and moveToRightmost().  moveToLeftmost()
** finds the left-most entry beneath the *entry* whereas moveToRightmost()
** finds the right-most entry beneath the *page*.
**
** The right-most entry is the one with the largest key - the last
** key in ascending order.
*/
static int moveToRightmost(BtCursor *pCur){
  Pgno pgno;
  int rc = SQLITE_OK;
  MemPage *pPage = 0;

  assert( cursorOwnsBtShared(pCur) );
  assert( pCur->eState==CURSOR_VALID );
  while( !(pPage = pCur->pPage)->leaf ){
    pgno = get4byte(&pPage->aData[pPage->hdrOffset+8]);
    pCur->ix = pPage->nCell;
    rc = moveToChild(pCur, pgno);
    if( rc ) return rc;
  }
  pCur->ix = pPage->nCell-1;
  assert( pCur->info.nSize==0 );
  assert( (pCur->curFlags & BTCF_ValidNKey)==0 );
  return SQLITE_OK;
}

/* Move the cursor to the first entry in the table.  Return SQLITE_OK
** on success.  Set *pRes to 0 if the cursor actually points to something
** or set *pRes to 1 if the table is empty.
*/
int sqlite3BtreeFirst(BtCursor *pCur, int *pRes){
  int rc;

  assert( cursorOwnsBtShared(pCur) );
  assert( sqlite3_mutex_held(pCur->pBtree->db->mutex) );
  rc = moveToRoot(pCur);
  if( rc==SQLITE_OK ){
    assert( pCur->pPage->nCell>0 );
    *pRes = 0;
    rc = moveToLeftmost(pCur);
  }else if( rc==SQLITE_EMPTY ){
    assert( pCur->pgnoRoot==0 || pCur->pPage->nCell==0 );
    *pRes = 1;
    rc = SQLITE_OK;
  }
  return rc;
}

// 移动光标到表的最后一条数据，如果表数据为空，*pRes = 1, 否则为0
static SQLITE_NOINLINE int btreeLast(BtCursor *pCur, int *pRes){
  int rc = moveToRoot(pCur);
  if( rc==SQLITE_OK ){
    assert( pCur->eState==CURSOR_VALID );
    *pRes = 0;
    rc = moveToRightmost(pCur);
    if( rc==SQLITE_OK ){
      pCur->curFlags |= BTCF_AtLast;
    }else{
      pCur->curFlags &= ~BTCF_AtLast;
    }
  }else if( rc==SQLITE_EMPTY ){
    assert( pCur->pgnoRoot==0 || pCur->pPage->nCell==0 );
    *pRes = 1;
    rc = SQLITE_OK;
  }
  return rc;
}
int sqlite3BtreeLast(BtCursor *pCur, int *pRes){
  assert( cursorOwnsBtShared(pCur) );
  assert( sqlite3_mutex_held(pCur->pBtree->db->mutex) );

  /* If the cursor already points to the last entry, this is a no-op. */
  if( CURSOR_VALID==pCur->eState && (pCur->curFlags & BTCF_AtLast)!=0 ){
#ifdef SQLITE_DEBUG
    /* This block serves to assert() that the cursor really does point 
    ** to the last entry in the b-tree. */
    int ii;
    for(ii=0; ii<pCur->iPage; ii++){
      assert( pCur->aiIdx[ii]==pCur->apPage[ii]->nCell );
    }
    assert( pCur->ix==pCur->pPage->nCell-1 || CORRUPT_DB );
    testcase( pCur->ix!=pCur->pPage->nCell-1 );
    /* ^-- dbsqlfuzz b92b72e4de80b5140c30ab71372ca719b8feb618 */
    assert( pCur->pPage->leaf );
#endif
    *pRes = 0;
    return SQLITE_OK;
  }
  return btreeLast(pCur, pRes);
}

/* Move the cursor so that it points to an entry in a table (a.k.a INTKEY)
** table near the key intKey.   Return a success code.
**
** If an exact match is not found, then the cursor is always
** left pointing at a leaf page which would hold the entry if it
** were present.  The cursor might point to an entry that comes
** before or after the key.
**
** An integer is written into *pRes which is the result of
** comparing the key with the entry to which the cursor is 
** pointing.  The meaning of the integer written into
** *pRes is as follows:
**
**     *pRes<0      The cursor is left pointing at an entry that
**                  is smaller than intKey or if the table is empty
**                  and the cursor is therefore left point to nothing.
**
**     *pRes==0     The cursor is left pointing at an entry that
**                  exactly matches intKey.
**
**     *pRes>0      The cursor is left pointing at an entry that
**                  is larger than intKey.
*/
int sqlite3BtreeTableMoveto(
  BtCursor *pCur,          /* The cursor to be moved */
  i64 intKey,              /* The table key */
  int biasRight,           /* If true, bias the search to the high end */
  int *pRes                /* Write search results here */
){
  int rc;

  assert( cursorOwnsBtShared(pCur) );
  assert( sqlite3_mutex_held(pCur->pBtree->db->mutex) );
  assert( pRes );
  assert( pCur->pKeyInfo==0 );
  assert( pCur->eState!=CURSOR_VALID || pCur->curIntKey!=0 );

  /* If the cursor is already positioned at the point we are trying
  ** to move to, then just return without doing any work */
  if( pCur->eState==CURSOR_VALID && (pCur->curFlags & BTCF_ValidNKey)!=0 ){
    if( pCur->info.nKey==intKey ){
      *pRes = 0;
      return SQLITE_OK;
    }
    if( pCur->info.nKey<intKey ){
      if( (pCur->curFlags & BTCF_AtLast)!=0 ){
        *pRes = -1;
        return SQLITE_OK;
      }
      /* If the requested key is one more than the previous key, then
      ** try to get there using sqlite3BtreeNext() rather than a full
      ** binary search.  This is an optimization only.  The correct answer
      ** is still obtained without this case, only a little more slowely */
      if( pCur->info.nKey+1==intKey ){
        *pRes = 0;
        rc = sqlite3BtreeNext(pCur, 0);
        if( rc==SQLITE_OK ){
          getCellInfo(pCur);
          if( pCur->info.nKey==intKey ){
            return SQLITE_OK;
          }
        }else if( rc!=SQLITE_DONE ){
          return rc;
        }
      }
    }
  }

#ifdef SQLITE_DEBUG
  pCur->pBtree->nSeek++;   /* Performance measurement during testing */
#endif

  rc = moveToRoot(pCur);
  if( rc ){
    if( rc==SQLITE_EMPTY ){
      assert( pCur->pgnoRoot==0 || pCur->pPage->nCell==0 );
      *pRes = -1;
      return SQLITE_OK;
    }
    return rc;
  }
  assert( pCur->pPage );
  assert( pCur->pPage->isInit );
  assert( pCur->eState==CURSOR_VALID );
  assert( pCur->pPage->nCell > 0 );
  assert( pCur->iPage==0 || pCur->apPage[0]->intKey==pCur->curIntKey );
  assert( pCur->curIntKey );

  for(;;){
    int lwr, upr, idx, c;
    Pgno chldPg;
    MemPage *pPage = pCur->pPage;
    u8 *pCell;                          /* Pointer to current cell in pPage */

    /* pPage->nCell must be greater than zero. If this is the root-page
    ** the cursor would have been INVALID above and this for(;;) loop
    ** not run. If this is not the root-page, then the moveToChild() routine
    ** would have already detected db corruption. Similarly, pPage must
    ** be the right kind (index or table) of b-tree page. Otherwise
    ** a moveToChild() or moveToRoot() call would have detected corruption.  */
    assert( pPage->nCell>0 );
    assert( pPage->intKey );
    lwr = 0;
    upr = pPage->nCell-1;
    assert( biasRight==0 || biasRight==1 );
    idx = upr>>(1-biasRight); /* idx = biasRight ? upr : (lwr+upr)/2; */
    for(;;){
      i64 nCellKey;
      pCell = findCellPastPtr(pPage, idx);
      if( pPage->intKeyLeaf ){
        while( 0x80 <= *(pCell++) ){
          if( pCell>=pPage->aDataEnd ){
            return SQLITE_CORRUPT_PAGE(pPage);
          }
        }
      }
      getVarint(pCell, (u64*)&nCellKey);
      if( nCellKey<intKey ){
        lwr = idx+1;
        if( lwr>upr ){ c = -1; break; }
      }else if( nCellKey>intKey ){
        upr = idx-1;
        if( lwr>upr ){ c = +1; break; }
      }else{
        assert( nCellKey==intKey );
        pCur->ix = (u16)idx;
        if( !pPage->leaf ){
          lwr = idx;
          goto moveto_table_next_layer;
        }else{
          pCur->curFlags |= BTCF_ValidNKey;
          pCur->info.nKey = nCellKey;
          pCur->info.nSize = 0;
          *pRes = 0;
          return SQLITE_OK;
        }
      }
      assert( lwr+upr>=0 );
      idx = (lwr+upr)>>1;  /* idx = (lwr+upr)/2; */
    }
    assert( lwr==upr+1 || !pPage->leaf );
    assert( pPage->isInit );
    if( pPage->leaf ){
      assert( pCur->ix<pCur->pPage->nCell );
      pCur->ix = (u16)idx;
      *pRes = c;
      rc = SQLITE_OK;
      goto moveto_table_finish;
    }
moveto_table_next_layer:
    if( lwr>=pPage->nCell ){
      chldPg = get4byte(&pPage->aData[pPage->hdrOffset+8]);
    }else{
      chldPg = get4byte(findCell(pPage, lwr));
    }
    pCur->ix = (u16)lwr;
    rc = moveToChild(pCur, chldPg);
    if( rc ) break;
  }
moveto_table_finish:
  pCur->info.nSize = 0;
  assert( (pCur->curFlags & BTCF_ValidOvfl)==0 );
  return rc;
}

/*
** Compare the "idx"-th cell on the page the cursor pCur is currently
** pointing to to pIdxKey using xRecordCompare.  Return negative or
** zero if the cell is less than or equal pIdxKey.  Return positive
** if unknown.
**
**    Return value negative:     Cell at pCur[idx] less than pIdxKey
**
**    Return value is zero:      Cell at pCur[idx] equals pIdxKey
**
**    Return value positive:     Nothing is known about the relationship
**                               of the cell at pCur[idx] and pIdxKey.
**
** This routine is part of an optimization.  It is always safe to return
** a positive value as that will cause the optimization to be skipped.
*/
static int indexCellCompare(
  BtCursor *pCur,
  int idx,
  UnpackedRecord *pIdxKey,
  RecordCompare xRecordCompare
){
  MemPage *pPage = pCur->pPage;
  int c;
  int nCell;  /* Size of the pCell cell in bytes */
  u8 *pCell = findCellPastPtr(pPage, idx);

  nCell = pCell[0];
  if( nCell<=pPage->max1bytePayload ){
    /* This branch runs if the record-size field of the cell is a
    ** single byte varint and the record fits entirely on the main
    ** b-tree page.  */
    testcase( pCell+nCell+1==pPage->aDataEnd );
    c = xRecordCompare(nCell, (void*)&pCell[1], pIdxKey);
  }else if( !(pCell[1] & 0x80) 
    && (nCell = ((nCell&0x7f)<<7) + pCell[1])<=pPage->maxLocal
  ){
    /* The record-size field is a 2 byte varint and the record 
    ** fits entirely on the main b-tree page.  */
    testcase( pCell+nCell+2==pPage->aDataEnd );
    c = xRecordCompare(nCell, (void*)&pCell[2], pIdxKey);
  }else{
    /* If the record extends into overflow pages, do not attempt
    ** the optimization. */
    c = 99;
  }
  return c;
}

/*
** Return true (non-zero) if pCur is current pointing to the last
** page of a table.
*/
static int cursorOnLastPage(BtCursor *pCur){
  int i;
  assert( pCur->eState==CURSOR_VALID );
  for(i=0; i<pCur->iPage; i++){
    MemPage *pPage = pCur->apPage[i];
    if( pCur->aiIdx[i]<pPage->nCell ) return 0;
  }
  return 1;
}

/* Move the cursor so that it points to an entry in an index table
** near the key pIdxKey.   Return a success code.
**
** If an exact match is not found, then the cursor is always
** left pointing at a leaf page which would hold the entry if it
** were present.  The cursor might point to an entry that comes
** before or after the key.
**
** An integer is written into *pRes which is the result of
** comparing the key with the entry to which the cursor is 
** pointing.  The meaning of the integer written into
** *pRes is as follows:
**
**     *pRes<0      The cursor is left pointing at an entry that
**                  is smaller than pIdxKey or if the table is empty
**                  and the cursor is therefore left point to nothing.
**
**     *pRes==0     The cursor is left pointing at an entry that
**                  exactly matches pIdxKey.
**
**     *pRes>0      The cursor is left pointing at an entry that
**                  is larger than pIdxKey.
**
** The pIdxKey->eqSeen field is set to 1 if there
** exists an entry in the table that exactly matches pIdxKey.  
*/
int sqlite3BtreeIndexMoveto(
  BtCursor *pCur,          /* The cursor to be moved */
  UnpackedRecord *pIdxKey, /* Unpacked index key */
  int *pRes                /* Write search results here */
){
  int rc;
  RecordCompare xRecordCompare;

  assert( cursorOwnsBtShared(pCur) );
  assert( sqlite3_mutex_held(pCur->pBtree->db->mutex) );
  assert( pRes );
  assert( pCur->pKeyInfo!=0 );

#ifdef SQLITE_DEBUG
  pCur->pBtree->nSeek++;   /* Performance measurement during testing */
#endif

  xRecordCompare = sqlite3VdbeFindCompare(pIdxKey);
  pIdxKey->errCode = 0;
  assert( pIdxKey->default_rc==1 
       || pIdxKey->default_rc==0 
       || pIdxKey->default_rc==-1
  );


  /* Check to see if we can skip a lot of work.  Two cases:
  **
  **    (1) If the cursor is already pointing to the very last cell
  **        in the table and the pIdxKey search key is greater than or
  **        equal to that last cell, then no movement is required.
  **
  **    (2) If the cursor is on the last page of the table and the first
  **        cell on that last page is less than or equal to the pIdxKey
  **        search key, then we can start the search on the current page
  **        without needing to go back to root.
  */
  if( pCur->eState==CURSOR_VALID
   && pCur->pPage->leaf
   && cursorOnLastPage(pCur)
  ){
    int c;
    if( pCur->ix==pCur->pPage->nCell-1
     && (c = indexCellCompare(pCur, pCur->ix, pIdxKey, xRecordCompare))<=0
     && pIdxKey->errCode==SQLITE_OK
    ){
      *pRes = c;
      return SQLITE_OK;  /* Cursor already pointing at the correct spot */
    }
    if( pCur->iPage>0 
     && indexCellCompare(pCur, 0, pIdxKey, xRecordCompare)<=0
     && pIdxKey->errCode==SQLITE_OK
    ){
      pCur->curFlags &= ~BTCF_ValidOvfl;
      if( !pCur->pPage->isInit ){
        return SQLITE_CORRUPT_BKPT;
      }
      goto bypass_moveto_root;  /* Start search on the current page */
    }
    pIdxKey->errCode = SQLITE_OK;
  }

  rc = moveToRoot(pCur);
  if( rc ){
    if( rc==SQLITE_EMPTY ){
      assert( pCur->pgnoRoot==0 || pCur->pPage->nCell==0 );
      *pRes = -1;
      return SQLITE_OK;
    }
    return rc;
  }

bypass_moveto_root:
  assert( pCur->pPage );
  assert( pCur->pPage->isInit );
  assert( pCur->eState==CURSOR_VALID );
  assert( pCur->pPage->nCell > 0 );
  assert( pCur->curIntKey==0 );
  assert( pIdxKey!=0 );
  for(;;){
    int lwr, upr, idx, c;
    Pgno chldPg;
    MemPage *pPage = pCur->pPage;
    u8 *pCell;                          /* Pointer to current cell in pPage */

    /* pPage->nCell must be greater than zero. If this is the root-page
    ** the cursor would have been INVALID above and this for(;;) loop
    ** not run. If this is not the root-page, then the moveToChild() routine
    ** would have already detected db corruption. Similarly, pPage must
    ** be the right kind (index or table) of b-tree page. Otherwise
    ** a moveToChild() or moveToRoot() call would have detected corruption.  */
    assert( pPage->nCell>0 );
    assert( pPage->intKey==0 );
    lwr = 0;
    upr = pPage->nCell-1;
    idx = upr>>1; /* idx = (lwr+upr)/2; */
    for(;;){
      int nCell;  /* Size of the pCell cell in bytes */
      pCell = findCellPastPtr(pPage, idx);

      /* The maximum supported page-size is 65536 bytes. This means that
      ** the maximum number of record bytes stored on an index B-Tree
      ** page is less than 16384 bytes and may be stored as a 2-byte
      ** varint. This information is used to attempt to avoid parsing 
      ** the entire cell by checking for the cases where the record is 
      ** stored entirely within the b-tree page by inspecting the first 
      ** 2 bytes of the cell.
      */
      nCell = pCell[0];
      if( nCell<=pPage->max1bytePayload ){
        /* This branch runs if the record-size field of the cell is a
        ** single byte varint and the record fits entirely on the main
        ** b-tree page.  */
        testcase( pCell+nCell+1==pPage->aDataEnd );
        c = xRecordCompare(nCell, (void*)&pCell[1], pIdxKey);
      }else if( !(pCell[1] & 0x80) 
        && (nCell = ((nCell&0x7f)<<7) + pCell[1])<=pPage->maxLocal
      ){
        /* The record-size field is a 2 byte varint and the record 
        ** fits entirely on the main b-tree page.  */
        testcase( pCell+nCell+2==pPage->aDataEnd );
        c = xRecordCompare(nCell, (void*)&pCell[2], pIdxKey);
      }else{
        /* The record flows over onto one or more overflow pages. In
        ** this case the whole cell needs to be parsed, a buffer allocated
        ** and accessPayload() used to retrieve the record into the
        ** buffer before VdbeRecordCompare() can be called. 
        **
        ** If the record is corrupt, the xRecordCompare routine may read
        ** up to two varints past the end of the buffer. An extra 18 
        ** bytes of padding is allocated at the end of the buffer in
        ** case this happens.  */
        void *pCellKey;
        u8 * const pCellBody = pCell - pPage->childPtrSize;
        const int nOverrun = 18;  /* Size of the overrun padding */
        pPage->xParseCell(pPage, pCellBody, &pCur->info);
        nCell = (int)pCur->info.nKey;
        testcase( nCell<0 );   /* True if key size is 2^32 or more */
        testcase( nCell==0 );  /* Invalid key size:  0x80 0x80 0x00 */
        testcase( nCell==1 );  /* Invalid key size:  0x80 0x80 0x01 */
        testcase( nCell==2 );  /* Minimum legal index key size */
        if( nCell<2 || nCell/pCur->pBt->usableSize>pCur->pBt->nPage ){
          rc = SQLITE_CORRUPT_PAGE(pPage);
          goto moveto_index_finish;
        }
        pCellKey = sqlite3Malloc( nCell+nOverrun );
        if( pCellKey==0 ){
          rc = SQLITE_NOMEM_BKPT;
          goto moveto_index_finish;
        }
        pCur->ix = (u16)idx;
        rc = accessPayload(pCur, 0, nCell, (unsigned char*)pCellKey, 0);
        memset(((u8*)pCellKey)+nCell,0,nOverrun); /* Fix uninit warnings */
        pCur->curFlags &= ~BTCF_ValidOvfl;
        if( rc ){
          sqlite3_free(pCellKey);
          goto moveto_index_finish;
        }
        c = sqlite3VdbeRecordCompare(nCell, pCellKey, pIdxKey);
        sqlite3_free(pCellKey);
      }
      assert( 
          (pIdxKey->errCode!=SQLITE_CORRUPT || c==0)
       && (pIdxKey->errCode!=SQLITE_NOMEM || pCur->pBtree->db->mallocFailed)
      );
      if( c<0 ){
        lwr = idx+1;
      }else if( c>0 ){
        upr = idx-1;
      }else{
        assert( c==0 );
        *pRes = 0;
        rc = SQLITE_OK;
        pCur->ix = (u16)idx;
        if( pIdxKey->errCode ) rc = SQLITE_CORRUPT_BKPT;
        goto moveto_index_finish;
      }
      if( lwr>upr ) break;
      assert( lwr+upr>=0 );
      idx = (lwr+upr)>>1;  /* idx = (lwr+upr)/2 */
    }
    assert( lwr==upr+1 || (pPage->intKey && !pPage->leaf) );
    assert( pPage->isInit );
    if( pPage->leaf ){
      assert( pCur->ix<pCur->pPage->nCell || CORRUPT_DB );
      pCur->ix = (u16)idx;
      *pRes = c;
      rc = SQLITE_OK;
      goto moveto_index_finish;
    }
    if( lwr>=pPage->nCell ){
      chldPg = get4byte(&pPage->aData[pPage->hdrOffset+8]);
    }else{
      chldPg = get4byte(findCell(pPage, lwr));
    }
    pCur->ix = (u16)lwr;
    rc = moveToChild(pCur, chldPg);
    if( rc ) break;
  }
moveto_index_finish:
  pCur->info.nSize = 0;
  assert( (pCur->curFlags & BTCF_ValidOvfl)==0 );
  return rc;
}


/*
** Return TRUE if the cursor is not pointing at an entry of the table.
**
** TRUE will be returned after a call to sqlite3BtreeNext() moves
** past the last entry in the table or sqlite3BtreePrev() moves past
** the first entry.  TRUE is also returned if the table is empty.
*/
int sqlite3BtreeEof(BtCursor *pCur){
  /* TODO: What if the cursor is in CURSOR_REQUIRESEEK but all table entries
  ** have been deleted? This API will need to change to return an error code
  ** as well as the boolean result value.
  */
  return (CURSOR_VALID!=pCur->eState);
}

/*
** Return an estimate for the number of rows in the table that pCur is
** pointing to.  Return a negative number if no estimate is currently 
** available.
*/
i64 sqlite3BtreeRowCountEst(BtCursor *pCur){
  i64 n;
  u8 i;

  assert( cursorOwnsBtShared(pCur) );
  assert( sqlite3_mutex_held(pCur->pBtree->db->mutex) );

  /* Currently this interface is only called by the OP_IfSmaller
  ** opcode, and it that case the cursor will always be valid and
  ** will always point to a leaf node. */
  if( NEVER(pCur->eState!=CURSOR_VALID) ) return -1;
  if( NEVER(pCur->pPage->leaf==0) ) return -1;

  n = pCur->pPage->nCell;
  for(i=0; i<pCur->iPage; i++){
    n *= pCur->apPage[i]->nCell;
  }
  return n;
}

/*
** Advance the cursor to the next entry in the database. 
** Return value:
**
**    SQLITE_OK        success
**    SQLITE_DONE      cursor is already pointing at the last element
**    otherwise        some kind of error occurred
**
** The main entry point is sqlite3BtreeNext().  That routine is optimized
** for the common case of merely incrementing the cell counter BtCursor.aiIdx
** to the next cell on the current page.  The (slower) btreeNext() helper
** routine is called when it is necessary to move to a different page or
** to restore the cursor.
**
** If bit 0x01 of the F argument in sqlite3BtreeNext(C,F) is 1, then the
** cursor corresponds to an SQL index and this routine could have been
** skipped if the SQL index had been a unique index.  The F argument
** is a hint to the implement.  SQLite btree implementation does not use
** this hint, but COMDB2 does.
*/
static SQLITE_NOINLINE int btreeNext(BtCursor *pCur){
  int rc;
  int idx;
  MemPage *pPage;

  assert( cursorOwnsBtShared(pCur) );
  if( pCur->eState!=CURSOR_VALID ){
    assert( (pCur->curFlags & BTCF_ValidOvfl)==0 );
    rc = restoreCursorPosition(pCur);
    if( rc!=SQLITE_OK ){
      return rc;
    }
    if( CURSOR_INVALID==pCur->eState ){
      return SQLITE_DONE;
    }
    if( pCur->eState==CURSOR_SKIPNEXT ){
      pCur->eState = CURSOR_VALID;
      if( pCur->skipNext>0 ) return SQLITE_OK;
    }
  }

  pPage = pCur->pPage;
  idx = ++pCur->ix;
  if( NEVER(!pPage->isInit) || sqlite3FaultSim(412) ){
    return SQLITE_CORRUPT_BKPT;
  }

  if( idx>=pPage->nCell ){
    if( !pPage->leaf ){
      rc = moveToChild(pCur, get4byte(&pPage->aData[pPage->hdrOffset+8]));
      if( rc ) return rc;
      return moveToLeftmost(pCur);
    }
    do{
      if( pCur->iPage==0 ){
        pCur->eState = CURSOR_INVALID;
        return SQLITE_DONE;
      }
      moveToParent(pCur);
      pPage = pCur->pPage;
    }while( pCur->ix>=pPage->nCell );
    if( pPage->intKey ){
      return sqlite3BtreeNext(pCur, 0);
    }else{
      return SQLITE_OK;
    }
  }
  if( pPage->leaf ){
    return SQLITE_OK;
  }else{
    return moveToLeftmost(pCur);
  }
}
int sqlite3BtreeNext(BtCursor *pCur, int flags){
  MemPage *pPage;
  UNUSED_PARAMETER( flags );  /* Used in COMDB2 but not native SQLite */
  assert( cursorOwnsBtShared(pCur) );
  assert( flags==0 || flags==1 );
  pCur->info.nSize = 0;
  pCur->curFlags &= ~(BTCF_ValidNKey|BTCF_ValidOvfl);
  if( pCur->eState!=CURSOR_VALID ) return btreeNext(pCur);
  pPage = pCur->pPage;
  if( (++pCur->ix)>=pPage->nCell ){
    pCur->ix--;
    return btreeNext(pCur);
  }
  if( pPage->leaf ){
    return SQLITE_OK;
  }else{
    return moveToLeftmost(pCur);
  }
}

/*
** Step the cursor to the back to the previous entry in the database.
** Return values:
**
**     SQLITE_OK     success
**     SQLITE_DONE   the cursor is already on the first element of the table
**     otherwise     some kind of error occurred
**
** The main entry point is sqlite3BtreePrevious().  That routine is optimized
** for the common case of merely decrementing the cell counter BtCursor.aiIdx
** to the previous cell on the current page.  The (slower) btreePrevious()
** helper routine is called when it is necessary to move to a different page
** or to restore the cursor.
**
** If bit 0x01 of the F argument to sqlite3BtreePrevious(C,F) is 1, then
** the cursor corresponds to an SQL index and this routine could have been
** skipped if the SQL index had been a unique index.  The F argument is a
** hint to the implement.  The native SQLite btree implementation does not
** use this hint, but COMDB2 does.
*/
static SQLITE_NOINLINE int btreePrevious(BtCursor *pCur){
  int rc;
  MemPage *pPage;

  assert( cursorOwnsBtShared(pCur) );
  assert( (pCur->curFlags & (BTCF_AtLast|BTCF_ValidOvfl|BTCF_ValidNKey))==0 );
  assert( pCur->info.nSize==0 );
  if( pCur->eState!=CURSOR_VALID ){
    rc = restoreCursorPosition(pCur);
    if( rc!=SQLITE_OK ){
      return rc;
    }
    if( CURSOR_INVALID==pCur->eState ){
      return SQLITE_DONE;
    }
    if( CURSOR_SKIPNEXT==pCur->eState ){
      pCur->eState = CURSOR_VALID;
      if( pCur->skipNext<0 ) return SQLITE_OK;
    }
  }

  pPage = pCur->pPage;
  assert( pPage->isInit );
  if( !pPage->leaf ){
    int idx = pCur->ix;
    rc = moveToChild(pCur, get4byte(findCell(pPage, idx)));
    if( rc ) return rc;
    rc = moveToRightmost(pCur);
  }else{
    while( pCur->ix==0 ){
      if( pCur->iPage==0 ){
        pCur->eState = CURSOR_INVALID;
        return SQLITE_DONE;
      }
      moveToParent(pCur);
    }
    assert( pCur->info.nSize==0 );
    assert( (pCur->curFlags & (BTCF_ValidOvfl))==0 );

    pCur->ix--;
    pPage = pCur->pPage;
    if( pPage->intKey && !pPage->leaf ){
      rc = sqlite3BtreePrevious(pCur, 0);
    }else{
      rc = SQLITE_OK;
    }
  }
  return rc;
}
int sqlite3BtreePrevious(BtCursor *pCur, int flags){
  assert( cursorOwnsBtShared(pCur) );
  assert( flags==0 || flags==1 );
  UNUSED_PARAMETER( flags );  /* Used in COMDB2 but not native SQLite */
  pCur->curFlags &= ~(BTCF_AtLast|BTCF_ValidOvfl|BTCF_ValidNKey);
  pCur->info.nSize = 0;
  if( pCur->eState!=CURSOR_VALID
   || pCur->ix==0
   || pCur->pPage->leaf==0
  ){
    return btreePrevious(pCur);
  }
  pCur->ix--;
  return SQLITE_OK;
}

/*
** 从数据库文件中分配一个新页面。
** 新页面会被标记为脏页（即已对新页面调用过 sqlite3PagerWrite()）。
** 同时，新页面的引用计数已被增加，调用方需在使用完毕后调用 sqlite3PagerUnref() 释放引用。
** 成功时返回 SQLITE_OK，其他返回值表示错误。发生错误时，*ppPage 会被设为 NULL。
** 若 "nearby" 参数不为 0，则尝试分配一个接近 "nearby" 页号的新页面。
** 这有助于将逻辑相关的页面在数据库文件中物理相邻存储，从而提升访问性能。
** 若 eMode 参数为 BTALLOC_EXACT：当 "nearby" 页存在于空闲列表时，保证返回该页。
** 若 eMode 参数为 BTALLOC_LT：返回的页号将小于或等于 "nearby"（如果存在此类页面）。
** 若 eMode 参数为 BTALLOC_ANY：对返回的页号无任何限制。
*/
/* 
空闲页分配伪代码
searList = (eMode==BTALLOC_EXACT && nearby is a freePage) || eMode==BTALLOC_LE
do {
	if k == 0 && !searchList
		alloc trunk page
	else if searchList && iTrunk 是目标页
		if k == 0
			alloc trunk page
		else 
			第一个叶子节点升为trunk page
			alloc trunk page
	else if k > 0
		if nearby > 0
			closet = trunk page上最接近nearby的页（BTALLOC_LE模式找到比nearby小的页就行）
		else 
			closet = 0
			
		if !searchList || (pgNo[closet] == nearby || BTALLOC_LE模式下pgNo[closet] == BTALLOC_LE)
			分配pgNo[closet]
			移动trunk page内容，k = k - 1;
			searchList = 0;
			
	move to next trunk page
} while(searchList)
*/

static int allocateBtreePage(
  BtShared *pBt,         /* The btree */
  MemPage **ppPage,      /* Store pointer to the allocated page here */
  Pgno *pPgno,           /* Store the page number here */
  Pgno nearby,           /* Search for a page near this one */
  u8 eMode               /* BTALLOC_EXACT, BTALLOC_LT, or BTALLOC_ANY */
){
  MemPage *pPage1;
  int rc;
  u32 n;     /* Number of pages on the freelist */
  u32 k;     /* Number of leaves on the trunk of the freelist */
  MemPage *pTrunk = 0;
  MemPage *pPrevTrunk = 0;
  Pgno mxPage;     /* Total size of the database file */

  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( eMode==BTALLOC_ANY || (nearby>0 && IfNotOmitAV(pBt->autoVacuum)) );
  pPage1 = pBt->pPage1;
  mxPage = btreePagecount(pBt);
  n = get4byte(&pPage1->aData[36]); // offset = 36 存放空闲页总数
  testcase( n==mxPage-1 );
  if( n>=mxPage ){
    return SQLITE_CORRUPT_BKPT;
  }
  if( n>0 ) { // 如果有空闲页，先分配空闲页
    Pgno iTrunk;
    u8 searchList = 0; // searchList = false 此循环内的代码仅执行一次, 否则需要找到满足要求的页
    u32 nSearch = 0;   /* Count of the number of search attempts */
    
    // 若 eMode == BTALLOC_EXACT，并且通过指针映射查询到页面 'nearby' 位于空闲列表中，则会完整遍历整个空闲列表以定位该页面。
#ifndef SQLITE_OMIT_AUTOVACUUM
    if( eMode==BTALLOC_EXACT ){
      if( nearby<=mxPage ){
        u8 eType;
        assert( nearby>0 && pBt->autoVacuum);
        rc = ptrmapGet(pBt, nearby, &eType, 0);
        if( rc ) return rc;
        if( eType==PTRMAP_FREEPAGE ){
          searchList = 1;
        }
      }
    }else if( eMode==BTALLOC_LE ){
      searchList = 1;
    }
#endif

    rc = sqlite3PagerWrite(pPage1->pDbPage);
    if( rc ) return rc;
    put4byte(&pPage1->aData[36], n-1); // 空闲页总数减1

    do {
      pPrevTrunk = pTrunk;
      if( pPrevTrunk ){
        iTrunk = get4byte(&pPrevTrunk->aData[0]); // trunk page offset = 0处存储链表上下一个trunk page 页号
      }else{
        iTrunk = get4byte(&pPage1->aData[32]); // offset = 32 处存储第一个Trunk页号
      }
      testcase( iTrunk==mxPage );
      if( iTrunk>mxPage || nSearch++ > n ){ // 校验iTrunk 和nSearch 是否合法
        rc = SQLITE_CORRUPT_PGNO(pPrevTrunk ? pPrevTrunk->pgno : 1);
      }else{
        rc = btreeGetUnusedPage(pBt, iTrunk, &pTrunk, 0);
      }
      if( rc ){
        pTrunk = 0;
        goto end_allocate_page;
      }
      assert( pTrunk!=0 );
      assert( pTrunk->aData!=0 );
      k = get4byte(&pTrunk->aData[4]); // trunk page 的offset = 4 存放本页的叶子页面数量
      if( k==0 && !searchList ){
        // 如果trunk page上没有叶子页面，且对页号没有要求，把这个trunk page 分配出去
        assert( pPrevTrunk==0 );
        rc = sqlite3PagerWrite(pTrunk->pDbPage);
        if( rc ){
          goto end_allocate_page;
        }
        *pPgno = iTrunk;
        memcpy(&pPage1->aData[32], &pTrunk->aData[0], 4);
        *ppPage = pTrunk;
        pTrunk = 0;
        TRACE(("ALLOCATE: %d trunk - %d free pages left\n", *pPgno, n-1));
      } else if( k>(u32)(pBt->usableSize/4 - 2) ){ // 校验叶子页面数量是否不合法
        /* Value of k is out of range.  Database corruption */
        rc = SQLITE_CORRUPT_PGNO(iTrunk);
        goto end_allocate_page;
#ifndef SQLITE_OMIT_AUTOVACUUM
      }else if( searchList && (nearby==iTrunk || (iTrunk<nearby && eMode==BTALLOC_LE)) ){ 
        // 如果 搜索到nearby页或者 eMode = BTALLOC_LE模式下搜索到比nearby小的页
        *pPgno = iTrunk;
        *ppPage = pTrunk;
        searchList = 0;
        rc = sqlite3PagerWrite(pTrunk->pDbPage);
        if( rc ){
          goto end_allocate_page;
        }
        if( k==0 ){ // trunk 页没有叶子节点
          if( !pPrevTrunk ){
            memcpy(&pPage1->aData[32], &pTrunk->aData[0], 4);
          }else{
            rc = sqlite3PagerWrite(pPrevTrunk->pDbPage);
            if( rc!=SQLITE_OK ){
              goto end_allocate_page;
            }
            memcpy(&pPrevTrunk->aData[0], &pTrunk->aData[0], 4);
          }
        }else{ // trunk 页是需要的页，把第一个叶子节点变成trunk页，然后再把trunk页分配出去
          MemPage *pNewTrunk;
          Pgno iNewTrunk = get4byte(&pTrunk->aData[8]);
          if( iNewTrunk>mxPage ){ 
            rc = SQLITE_CORRUPT_PGNO(iTrunk);
            goto end_allocate_page;
          }
          testcase( iNewTrunk==mxPage );
          rc = btreeGetUnusedPage(pBt, iNewTrunk, &pNewTrunk, 0);
          if( rc!=SQLITE_OK ){
            goto end_allocate_page;
          }
          rc = sqlite3PagerWrite(pNewTrunk->pDbPage);
          if( rc!=SQLITE_OK ){
            releasePage(pNewTrunk);
            goto end_allocate_page;
          }
          memcpy(&pNewTrunk->aData[0], &pTrunk->aData[0], 4);
          put4byte(&pNewTrunk->aData[4], k-1);
          memcpy(&pNewTrunk->aData[8], &pTrunk->aData[12], (k-1)*4);
          releasePage(pNewTrunk);
          if( !pPrevTrunk ){
            assert( sqlite3PagerIswriteable(pPage1->pDbPage) );
            put4byte(&pPage1->aData[32], iNewTrunk);
          }else{
            rc = sqlite3PagerWrite(pPrevTrunk->pDbPage);
            if( rc ){
              goto end_allocate_page;
            }
            put4byte(&pPrevTrunk->aData[0], iNewTrunk);
          }
        }
        pTrunk = 0;
        TRACE(("ALLOCATE: %d trunk - %d free pages left\n", *pPgno, n-1));
#endif
      } else if( k>0 ) { // trunk页有叶子节点
        u32 closest;
        Pgno iPage;
        unsigned char *aData = pTrunk->aData;
        if( nearby>0 ){
          u32 i;
          closest = 0;
          if( eMode==BTALLOC_LE ){
            for(i=0; i<k; i++){
              iPage = get4byte(&aData[8+i*4]);
              if( iPage<=nearby ){
                closest = i;
                break;
              }
            }
          }else{
            int dist;
            dist = sqlite3AbsInt32(get4byte(&aData[8]) - nearby);
            for(i=1; i<k; i++){
              int d2 = sqlite3AbsInt32(get4byte(&aData[8+i*4]) - nearby);
              if( d2<dist ){
                closest = i;
                dist = d2;
              }
            }
          }
        }else{
          closest = 0;
        }

        iPage = get4byte(&aData[8+closest*4]);
        testcase( iPage==mxPage );
        if( iPage>mxPage || iPage<2 ){
          rc = SQLITE_CORRUPT_PGNO(iTrunk);
          goto end_allocate_page;
        }
        testcase( iPage==mxPage );
        if( !searchList 
         || (iPage==nearby || (iPage<nearby && eMode==BTALLOC_LE)) 
        ){
          int noContent;
          *pPgno = iPage;
          TRACE(("ALLOCATE: %d was leaf %d of %d on trunk %d"
                 ": %d more free pages\n",
                 *pPgno, closest+1, k, pTrunk->pgno, n-1));
          rc = sqlite3PagerWrite(pTrunk->pDbPage);
          if( rc ) goto end_allocate_page;
          if( closest<k-1 ){
            memcpy(&aData[8+closest*4], &aData[4+k*4], 4);
          }
          put4byte(&aData[4], k-1);
          noContent = !btreeGetHasContent(pBt, *pPgno)? PAGER_GET_NOCONTENT : 0;
          rc = btreeGetUnusedPage(pBt, *pPgno, ppPage, noContent);
          if( rc==SQLITE_OK ){
            rc = sqlite3PagerWrite((*ppPage)->pDbPage);
            if( rc!=SQLITE_OK ){
              releasePage(*ppPage);
              *ppPage = 0;
            }
          }
          searchList = 0;
        }
      }
      releasePage(pPrevTrunk);
      pPrevTrunk = 0;
    }while( searchList );
  }else{
    /* 
    ** 空闲列表中没有可用页面，因此向数据库映像追加一个新页。
    ** 
    ** 通常，通过此代码块分配的新页可以从页缓存层（pager layer）以 'no-content'（无内容）标志请求，
    ** 这会阻止页缓存层尝试从磁盘读取该页内容。但是，如果当前事务已经执行了一个或多个增量清理（incremental-vacuum）步骤，
    ** 则即将分配的页可能包含回滚时需要的内容。此时，禁止设置 no-content 标志，这会强制页缓存层在覆盖该页前加载其当前内容并写入日志。
    **
    ** 注意：页缓存层实际上不会尝试加载或记录那些确实位于磁盘数据库文件末尾之后的页的内容。
    ** 因此，禁用 no-content 优化的影响仅限于那些位于数据库映像末尾和数据库文件末尾之间的页。
    */
    int bNoContent = (0==IfNotOmitAV(pBt->bDoTruncate))? PAGER_GET_NOCONTENT:0;

    rc = sqlite3PagerWrite(pBt->pPage1->pDbPage);
    if( rc ) return rc;
    pBt->nPage++;
    if( pBt->nPage==PENDING_BYTE_PAGE(pBt) ) pBt->nPage++;

#ifndef SQLITE_OMIT_AUTOVACUUM
    if( pBt->autoVacuum && PTRMAP_ISPAGE(pBt, pBt->nPage) ){
      /* If *pPgno refers to a pointer-map page, allocate two new pages
      ** at the end of the file instead of one. The first allocated page
      ** becomes a new pointer-map page, the second is used by the caller.
      */
      MemPage *pPg = 0;
      TRACE(("ALLOCATE: %d from end of file (pointer-map page)\n", pBt->nPage));
      assert( pBt->nPage!=PENDING_BYTE_PAGE(pBt) );
      rc = btreeGetUnusedPage(pBt, pBt->nPage, &pPg, bNoContent);
      if( rc==SQLITE_OK ){
        rc = sqlite3PagerWrite(pPg->pDbPage);
        releasePage(pPg);
      }
      if( rc ) return rc;
      pBt->nPage++;
      if( pBt->nPage==PENDING_BYTE_PAGE(pBt) ){ pBt->nPage++; }
    }
#endif
    put4byte(28 + (u8*)pBt->pPage1->aData, pBt->nPage);
    *pPgno = pBt->nPage;

    assert( *pPgno!=PENDING_BYTE_PAGE(pBt) );
    rc = btreeGetUnusedPage(pBt, *pPgno, ppPage, bNoContent);
    if( rc ) return rc;
    rc = sqlite3PagerWrite((*ppPage)->pDbPage);
    if( rc!=SQLITE_OK ){
      releasePage(*ppPage);
      *ppPage = 0;
    }
    TRACE(("ALLOCATE: %d from end of file\n", *pPgno));
  }

  assert( CORRUPT_DB || *pPgno!=PENDING_BYTE_PAGE(pBt) );

end_allocate_page:
  releasePage(pTrunk);
  releasePage(pPrevTrunk);
  assert( rc!=SQLITE_OK || sqlite3PagerPageRefcount((*ppPage)->pDbPage)<=1 );
  assert( rc!=SQLITE_OK || (*ppPage)->isInit==0 );
  return rc;
}

/*
** 此函数用于将页号为 iPage 的页面添加到数据库文件的空闲列表（free-list）中。
** 此函数假定该页面尚未处于空闲列表内。
**
** 传递给此函数的第二个参数是可选的。如果调用方恰好持有与页 iPage 对应的 
** MemPage 对象的指针，可将其作为第二个参数传递；否则，可传递 NULL。
**
** 若传递了 MemPage 对象指针作为第二个参数，此函数不会改变其引用计数。
*/
static int freePage2(BtShared *pBt, MemPage *pMemPage, Pgno iPage){
  MemPage *pTrunk = 0;                /* Free-list trunk page */
  Pgno iTrunk = 0;                    /* Page number of free-list trunk page */ 
  MemPage *pPage1 = pBt->pPage1;      /* Local reference to page 1 */
  MemPage *pPage;                     /* Page being freed. May be NULL. */
  int rc;                             /* Return Code */
  u32 nFree;                          /* Initial number of pages on free-list */

  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( CORRUPT_DB || iPage>1 );
  assert( !pMemPage || pMemPage->pgno==iPage );

  if( iPage<2 || iPage>pBt->nPage ){
    return SQLITE_CORRUPT_BKPT;
  }
  if( pMemPage ){
    pPage = pMemPage;
    sqlite3PagerRef(pPage->pDbPage);
  }else{
    pPage = btreePageLookup(pBt, iPage);
  }

  /* Increment the free page count on pPage1 */
  rc = sqlite3PagerWrite(pPage1->pDbPage);
  if( rc ) goto freepage_out;
  nFree = get4byte(&pPage1->aData[36]);
  put4byte(&pPage1->aData[36], nFree+1);

  if( pBt->btsFlags & BTS_SECURE_DELETE ){
    /* If the secure_delete option is enabled, then
    ** always fully overwrite deleted information with zeros.
    */
    if( (!pPage && ((rc = btreeGetPage(pBt, iPage, &pPage, 0))!=0) )
     ||            ((rc = sqlite3PagerWrite(pPage->pDbPage))!=0)
    ){
      goto freepage_out;
    }
    memset(pPage->aData, 0, pPage->pBt->pageSize);
  }

  /* If the database supports auto-vacuum, write an entry in the pointer-map
  ** to indicate that the page is free.
  */
  if( ISAUTOVACUUM(pBt) ){
    ptrmapPut(pBt, iPage, PTRMAP_FREEPAGE, 0, &rc);
    if( rc ) goto freepage_out;
  }

  /* 
  ** 接下来操作实际的数据库空闲列表结构。存在两种可能情况：
  **
  ** 1. 若当前空闲列表为空，或空闲列表中的第一个主干页已满，
  **    则此页将成为一个新的空闲列表主干页。
  **
  ** 2. 否则，此页将成为当前空闲列表第一个主干页的一个叶子页。
  ** 
  ** 此代码块测试是否可以将该页作为新叶子页添加到当前空闲列表的首个主干页中。
  */
  if( nFree!=0 ){
    u32 nLeaf;                /* Initial number of leaf cells on trunk page */

    iTrunk = get4byte(&pPage1->aData[32]);
    if( iTrunk>btreePagecount(pBt) ){
      rc = SQLITE_CORRUPT_BKPT;
      goto freepage_out;
    }
    rc = btreeGetPage(pBt, iTrunk, &pTrunk, 0);
    if( rc!=SQLITE_OK ){
      goto freepage_out;
    }

    nLeaf = get4byte(&pTrunk->aData[4]);
    assert( pBt->usableSize>32 );
    if( nLeaf > (u32)pBt->usableSize/4 - 2 ){
      rc = SQLITE_CORRUPT_BKPT;
      goto freepage_out;
    }
    if( nLeaf < (u32)pBt->usableSize/4 - 8 ){
    /* 
    ** 在这种情况下，主干页（trunk page）仍有空间将待释放的页作为新叶子页插入。
    **
    ** 注意：实际上，主干页直到包含 usableSize/4 - 2 个条目时才算真正满（而不是我们现在
    ** 代码中使用的 usableSize/4 - 8 条目）。但由于 SQLite 3.6.0 之前版本存在代码错误，
    ** 当主干页包含超过 usableSize/4 - 8 个条目时，数据库会被旧版本报告为损坏。为了保持与
    ** 旧版 SQLite 的向后兼容性，我们暂时继续将条目数量限制为 usableSize/4 - 8。
    ** 在未来的某个时刻（当所有用户都升级到 3.6.0 或更高版本后），应考虑将上述条件判断中的
    ** "usableSize/4-8" 修正为 "usableSize/4-2"。
    **
    ** 证据标记 EVIDENCE-OF: R-19920-11576 
    ** 新版本 SQLite 仍然避免使用空闲列表主干页数组的最后 6 个条目，
    ** 以确保新版本创建的数据库文件可被旧版本 SQLite 读取。
    */
      rc = sqlite3PagerWrite(pTrunk->pDbPage);
      if( rc==SQLITE_OK ){
        put4byte(&pTrunk->aData[4], nLeaf+1);
        put4byte(&pTrunk->aData[8+nLeaf*4], iPage);
        if( pPage && (pBt->btsFlags & BTS_SECURE_DELETE)==0 ){
          sqlite3PagerDontWrite(pPage->pDbPage);
        }
        rc = btreeSetHasContent(pBt, iPage);
      }
      TRACE(("FREE-PAGE: %d leaf on trunk page %d\n",pPage->pgno,pTrunk->pgno));
      goto freepage_out;
    }
  }

  /* If control flows to this point, then it was not possible to add the
  ** the page being freed as a leaf page of the first trunk in the free-list.
  ** Possibly because the free-list is empty, or possibly because the 
  ** first trunk in the free-list is full. Either way, the page being freed
  ** will become the new first trunk page in the free-list.
  */
  if( pPage==0 && SQLITE_OK!=(rc = btreeGetPage(pBt, iPage, &pPage, 0)) ){
    goto freepage_out;
  }
  rc = sqlite3PagerWrite(pPage->pDbPage);
  if( rc!=SQLITE_OK ){
    goto freepage_out;
  }
  put4byte(pPage->aData, iTrunk);
  put4byte(&pPage->aData[4], 0);
  put4byte(&pPage1->aData[32], iPage);
  TRACE(("FREE-PAGE: %d new trunk page replacing %d\n", pPage->pgno, iTrunk));

freepage_out:
  if( pPage ){
    pPage->isInit = 0;
  }
  releasePage(pPage);
  releasePage(pTrunk);
  return rc;
}
static void freePage(MemPage *pPage, int *pRC){
  if( (*pRC)==SQLITE_OK ){
    *pRC = freePage2(pPage->pBt, pPage, pPage->pgno);
  }
}

/*
** Free the overflow pages associated with the given Cell.
*/
static SQLITE_NOINLINE int clearCellOverflow(
  MemPage *pPage,          /* The page that contains the Cell */
  unsigned char *pCell,    /* First byte of the Cell */
  CellInfo *pInfo          /* Size information about the cell */
){
  BtShared *pBt;
  Pgno ovflPgno;
  int rc;
  int nOvfl;
  u32 ovflPageSize;

  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( pInfo->nLocal!=pInfo->nPayload );
  testcase( pCell + pInfo->nSize == pPage->aDataEnd );
  testcase( pCell + (pInfo->nSize-1) == pPage->aDataEnd );
  if( pCell + pInfo->nSize > pPage->aDataEnd ){
    /* Cell extends past end of page */
    return SQLITE_CORRUPT_PAGE(pPage);
  }
  ovflPgno = get4byte(pCell + pInfo->nSize - 4);
  pBt = pPage->pBt;
  assert( pBt->usableSize > 4 );
  ovflPageSize = pBt->usableSize - 4;
  nOvfl = (pInfo->nPayload - pInfo->nLocal + ovflPageSize - 1)/ovflPageSize;
  assert( nOvfl>0 || 
    (CORRUPT_DB && (pInfo->nPayload + ovflPageSize)<ovflPageSize)
  );
  while( nOvfl-- ){
    Pgno iNext = 0;
    MemPage *pOvfl = 0;
    if( ovflPgno<2 || ovflPgno>btreePagecount(pBt) ){
      /* 0 is not a legal page number and page 1 cannot be an 
      ** overflow page. Therefore if ovflPgno<2 or past the end of the 
      ** file the database must be corrupt. */
      return SQLITE_CORRUPT_BKPT;
    }
    if( nOvfl ){
      rc = getOverflowPage(pBt, ovflPgno, &pOvfl, &iNext);
      if( rc ) return rc;
    }

    if( ( pOvfl || ((pOvfl = btreePageLookup(pBt, ovflPgno))!=0) )
     && sqlite3PagerPageRefcount(pOvfl->pDbPage)!=1
    ){
      /* There is no reason any cursor should have an outstanding reference 
      ** to an overflow page belonging to a cell that is being deleted/updated.
      ** So if there exists more than one reference to this page, then it 
      ** must not really be an overflow page and the database must be corrupt. 
      ** It is helpful to detect this before calling freePage2(), as 
      ** freePage2() may zero the page contents if secure-delete mode is
      ** enabled. If this 'overflow' page happens to be a page that the
      ** caller is iterating through or using in some other way, this
      ** can be problematic.
      */
      rc = SQLITE_CORRUPT_BKPT;
    }else{
      rc = freePage2(pBt, pOvfl, ovflPgno);
    }

    if( pOvfl ){
      sqlite3PagerUnref(pOvfl->pDbPage);
    }
    if( rc ) return rc;
    ovflPgno = iNext;
  }
  return SQLITE_OK;
}

/* Call xParseCell to compute the size of a cell.  If the cell contains
** overflow, then invoke cellClearOverflow to clear out that overflow.
** STore the result code (SQLITE_OK or some error code) in rc.
**
** Implemented as macro to force inlining for performance.
*/
#define BTREE_CLEAR_CELL(rc, pPage, pCell, sInfo)   \
  pPage->xParseCell(pPage, pCell, &sInfo);          \
  if( sInfo.nLocal!=sInfo.nPayload ){               \
    rc = clearCellOverflow(pPage, pCell, &sInfo);   \
  }else{                                            \
    rc = SQLITE_OK;                                 \
  }


/*
** Create the byte sequence used to represent a cell on page pPage
** and write that byte sequence into pCell[].  Overflow pages are
** allocated and filled in as necessary.  The calling procedure
** is responsible for making sure sufficient space has been allocated
** for pCell[].
**
** Note that pCell does not necessary need to point to the pPage->aData
** area.  pCell might point to some temporary storage.  The cell will
** be constructed in this temporary area then copied into pPage->aData
** later.
*/
static int fillInCell(
  MemPage *pPage,                /* The page that contains the cell */
  unsigned char *pCell,          /* Complete text of the cell */
  const BtreePayload *pX,        /* Payload with which to construct the cell */
  int *pnSize                    /* Write cell size here */
){
  int nPayload;
  const u8 *pSrc;
  int nSrc, n, rc, mn;
  int spaceLeft;
  MemPage *pToRelease;
  unsigned char *pPrior;
  unsigned char *pPayload;
  BtShared *pBt;
  Pgno pgnoOvfl;
  int nHeader;

  assert( sqlite3_mutex_held(pPage->pBt->mutex) );

  /* pPage is not necessarily writeable since pCell might be auxiliary
  ** buffer space that is separate from the pPage buffer area */
  assert( pCell<pPage->aData || pCell>=&pPage->aData[pPage->pBt->pageSize]
            || sqlite3PagerIswriteable(pPage->pDbPage) );

  /* Fill in the header. */
  nHeader = pPage->childPtrSize;
  if( pPage->intKey ){
    nPayload = pX->nData + pX->nZero;
    pSrc = pX->pData;
    nSrc = pX->nData;
    assert( pPage->intKeyLeaf ); /* fillInCell() only called for leaves */
    nHeader += putVarint32(&pCell[nHeader], nPayload);
    nHeader += putVarint(&pCell[nHeader], *(u64*)&pX->nKey);
  }else{
    assert( pX->nKey<=0x7fffffff && pX->pKey!=0 );
    nSrc = nPayload = (int)pX->nKey;
    pSrc = pX->pKey;
    nHeader += putVarint32(&pCell[nHeader], nPayload);
  }
  
  /* Fill in the payload */
  pPayload = &pCell[nHeader];
  if( nPayload<=pPage->maxLocal ){
    /* This is the common case where everything fits on the btree page
    ** and no overflow pages are required. */
    n = nHeader + nPayload;
    testcase( n==3 );
    testcase( n==4 );
    if( n<4 ) n = 4;
    *pnSize = n;
    assert( nSrc<=nPayload );
    testcase( nSrc<nPayload );
    memcpy(pPayload, pSrc, nSrc);
    memset(pPayload+nSrc, 0, nPayload-nSrc);
    return SQLITE_OK;
  }

  /* If we reach this point, it means that some of the content will need
  ** to spill onto overflow pages.
  */
  mn = pPage->minLocal;
  n = mn + (nPayload - mn) % (pPage->pBt->usableSize - 4);
  testcase( n==pPage->maxLocal );
  testcase( n==pPage->maxLocal+1 );
  if( n > pPage->maxLocal ) n = mn;
  spaceLeft = n; // 写在cell上的
  *pnSize = n + nHeader + 4;
  pPrior = &pCell[nHeader+n];
  pToRelease = 0;
  pgnoOvfl = 0;
  pBt = pPage->pBt;

  /* At this point variables should be set as follows:
  **
  **   nPayload           Total payload size in bytes
  **   pPayload           Begin writing payload here
  **   spaceLeft          Space available at pPayload.  If nPayload>spaceLeft,
  **                      that means content must spill into overflow pages.
  **   *pnSize            Size of the local cell (not counting overflow pages)
  **   pPrior             Where to write the pgno of the first overflow page
  **
  ** Use a call to btreeParseCellPtr() to verify that the values above
  ** were computed correctly.
  */
#ifdef SQLITE_DEBUG
  {
    CellInfo info;
    pPage->xParseCell(pPage, pCell, &info);
    assert( nHeader==(int)(info.pPayload - pCell) );
    assert( info.nKey==pX->nKey );
    assert( *pnSize == info.nSize );
    assert( spaceLeft == info.nLocal );
  }
#endif

  /* Write the payload into the local Cell and any extra into overflow pages */
  while( 1 ){
    n = nPayload;
    if( n>spaceLeft ) n = spaceLeft;

    /* If pToRelease is not zero than pPayload points into the data area
    ** of pToRelease.  Make sure pToRelease is still writeable. */
    assert( pToRelease==0 || sqlite3PagerIswriteable(pToRelease->pDbPage) );

    /* If pPayload is part of the data area of pPage, then make sure pPage
    ** is still writeable */
    assert( pPayload<pPage->aData || pPayload>=&pPage->aData[pBt->pageSize]
            || sqlite3PagerIswriteable(pPage->pDbPage) );

    if( nSrc>=n ){
      memcpy(pPayload, pSrc, n);
    }else if( nSrc>0 ){
      n = nSrc;
      memcpy(pPayload, pSrc, n);
    }else{
      memset(pPayload, 0, n);
    }
    nPayload -= n;
    if( nPayload<=0 ) break;
    pPayload += n;
    pSrc += n;
    nSrc -= n;
    spaceLeft -= n;
    if( spaceLeft==0 ){
      MemPage *pOvfl = 0;
#ifndef SQLITE_OMIT_AUTOVACUUM
      Pgno pgnoPtrmap = pgnoOvfl; /* Overflow page pointer-map entry page */
      if( pBt->autoVacuum ){
        do{
          pgnoOvfl++;
        } while( 
          PTRMAP_ISPAGE(pBt, pgnoOvfl) || pgnoOvfl==PENDING_BYTE_PAGE(pBt) 
        );
      }
#endif
      rc = allocateBtreePage(pBt, &pOvfl, &pgnoOvfl, pgnoOvfl, 0);
#ifndef SQLITE_OMIT_AUTOVACUUM
      /* If the database supports auto-vacuum, and the second or subsequent
      ** overflow page is being allocated, add an entry to the pointer-map
      ** for that page now. 
      **
      ** If this is the first overflow page, then write a partial entry 
      ** to the pointer-map. If we write nothing to this pointer-map slot,
      ** then the optimistic overflow chain processing in clearCell()
      ** may misinterpret the uninitialized values and delete the
      ** wrong pages from the database.
      */
      if( pBt->autoVacuum && rc==SQLITE_OK ){
        u8 eType = (pgnoPtrmap?PTRMAP_OVERFLOW2:PTRMAP_OVERFLOW1);
        ptrmapPut(pBt, pgnoOvfl, eType, pgnoPtrmap, &rc);
        if( rc ){
          releasePage(pOvfl);
        }
      }
#endif
      if( rc ){
        releasePage(pToRelease);
        return rc;
      }

      /* If pToRelease is not zero than pPrior points into the data area
      ** of pToRelease.  Make sure pToRelease is still writeable. */
      assert( pToRelease==0 || sqlite3PagerIswriteable(pToRelease->pDbPage) );

      /* If pPrior is part of the data area of pPage, then make sure pPage
      ** is still writeable */
      assert( pPrior<pPage->aData || pPrior>=&pPage->aData[pBt->pageSize]
            || sqlite3PagerIswriteable(pPage->pDbPage) );

      put4byte(pPrior, pgnoOvfl);
      releasePage(pToRelease);
      pToRelease = pOvfl;
      pPrior = pOvfl->aData;
      put4byte(pPrior, 0);
      pPayload = &pOvfl->aData[4];
      spaceLeft = pBt->usableSize - 4;
    }
  }
  releasePage(pToRelease);
  return SQLITE_OK;
}

/*
** 从 pPage 中移除第 i 个单元格（Cell）。此例程仅影响 pPage 自身。
** 单元格的内容不会被释放（Freed）或解除分配（Deallocated）。调用者需确保
** 单元格内容已复制到其他位置。此例程仅移除 pPage 对该单元格的引用。
** 
** 参数 "sz" 必须是该单元格的字节数。
*/
static void dropCell(MemPage *pPage, int idx, int sz, int *pRC){
  u32 pc;         /* Offset to cell content of cell being deleted */
  u8 *data;       /* pPage->aData */
  u8 *ptr;        /* Used to move bytes around within data[] */
  int rc;         /* The return code */
  int hdr;        /* Beginning of the header.  0 most pages.  100 page 1 */

  if( *pRC ) return;
  assert( idx>=0 );
  assert( idx<pPage->nCell );
  assert( CORRUPT_DB || sz==cellSize(pPage, idx) );
  assert( sqlite3PagerIswriteable(pPage->pDbPage) );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( pPage->nFree>=0 );
  data = pPage->aData;
  ptr = &pPage->aCellIdx[2*idx];
  assert( pPage->pBt->usableSize > (u32)(ptr-data) );
  pc = get2byte(ptr);
  hdr = pPage->hdrOffset;
  testcase( pc==(u32)get2byte(&data[hdr+5]) );
  testcase( pc+sz==pPage->pBt->usableSize );
  if( pc+sz > pPage->pBt->usableSize ){
    *pRC = SQLITE_CORRUPT_BKPT;
    return;
  }
  rc = freeSpace(pPage, pc, sz);
  if( rc ){
    *pRC = rc;
    return;
  }
  pPage->nCell--;
  if( pPage->nCell==0 ){
    memset(&data[hdr+1], 0, 4);
    data[hdr+7] = 0;
    put2byte(&data[hdr+5], pPage->pBt->usableSize);
    pPage->nFree = pPage->pBt->usableSize - pPage->hdrOffset
                       - pPage->childPtrSize - 8;
  }else{
    memmove(ptr, ptr+2, 2*(pPage->nCell - idx));
    put2byte(&data[hdr+3], pPage->nCell);
    pPage->nFree += 2;
  }
}

/*
在页面pPage的单元格索引i处插入新单元格，单元格内容由pCell指针指向。
若当前页面可容纳该单元格，则直接写入页面存储区. 若无法容纳, 当pTemp非空时，将单元格内容复制至临时缓冲区pTemp.
在pPage->apOvfl[]数组中分配新条目，使其指向单元格内容并记录该溢出条目的索引号。
在pPage->aCell[]中分配新条目会导致pPage->nOverflow自增
*/
static int insertCell(
  MemPage *pPage,   /* Page into which we are copying */
  int i,            /* New cell becomes the i-th cell of the page */
  u8 *pCell,        /* Content of the new cell */
  int sz,           /* Bytes of content in pCell */
  u8 *pTemp,        /* Temp storage space for pCell, if needed */
  Pgno iChild       /* If non-zero, replace first 4 bytes with this value */
){
  int idx = 0;      /* Where to write new cell content in data[] */
  int j;            /* Loop counter */
  u8 *data;         /* The content of the whole page */
  u8 *pIns;         /* The point in pPage->aCellIdx[] where no cell inserted */

  assert( i>=0 && i<=pPage->nCell+pPage->nOverflow );
  assert( MX_CELL(pPage->pBt)<=10921 );
  assert( pPage->nCell<=MX_CELL(pPage->pBt) || CORRUPT_DB );
  assert( pPage->nOverflow<=ArraySize(pPage->apOvfl) );
  assert( ArraySize(pPage->apOvfl)==ArraySize(pPage->aiOvfl) );
  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( sz==pPage->xCellSize(pPage, pCell) || CORRUPT_DB );
  assert( pPage->nFree>=0 );
  if( pPage->nOverflow || sz+2>pPage->nFree ){
    if( pTemp ){
      memcpy(pTemp, pCell, sz);
      pCell = pTemp;
    }
    if( iChild ){
      put4byte(pCell, iChild);
    }
    j = pPage->nOverflow++;
    /* Comparison against ArraySize-1 since we hold back one extra slot
    ** as a contingency.  In other words, never need more than 3 overflow
    ** slots but 4 are allocated, just to be safe. */
    assert( j < ArraySize(pPage->apOvfl)-1 );
    pPage->apOvfl[j] = pCell;
    pPage->aiOvfl[j] = (u16)i;

    /* When multiple overflows occur, they are always sequential and in
    ** sorted order.  This invariants arise because multiple overflows can
    ** only occur when inserting divider cells into the parent page during
    ** balancing, and the dividers are adjacent and sorted.
    */
    assert( j==0 || pPage->aiOvfl[j-1]<(u16)i ); /* Overflows in sorted order */
    assert( j==0 || i==pPage->aiOvfl[j-1]+1 );   /* Overflows are sequential */
  }else{
    int rc = sqlite3PagerWrite(pPage->pDbPage);
    if( rc!=SQLITE_OK ){
      return rc;
    }
    assert( sqlite3PagerIswriteable(pPage->pDbPage) );
    data = pPage->aData;
    assert( &data[pPage->cellOffset]==pPage->aCellIdx );
    rc = allocateSpace(pPage, sz, &idx);
    if( rc ){ return rc; }
    /* The allocateSpace() routine guarantees the following properties
    ** if it returns successfully */
    assert( idx >= 0 );
    assert( idx >= pPage->cellOffset+2*pPage->nCell+2 || CORRUPT_DB );
    assert( idx+sz <= (int)pPage->pBt->usableSize );
    pPage->nFree -= (u16)(2 + sz);
    if( iChild ){
      /* In a corrupt database where an entry in the cell index section of
      ** a btree page has a value of 3 or less, the pCell value might point
      ** as many as 4 bytes in front of the start of the aData buffer for
      ** the source page.  Make sure this does not cause problems by not
      ** reading the first 4 bytes */
      memcpy(&data[idx+4], pCell+4, sz-4);
      put4byte(&data[idx], iChild);
    }else{
      memcpy(&data[idx], pCell, sz);
    }
    pIns = pPage->aCellIdx + i*2;
    memmove(pIns+2, pIns, 2*(pPage->nCell - i));
    put2byte(pIns, idx);
    pPage->nCell++;
    /* increment the cell count */
    if( (++data[pPage->hdrOffset+4])==0 ) data[pPage->hdrOffset+3]++;
    assert( get2byte(&data[pPage->hdrOffset+3])==pPage->nCell || CORRUPT_DB );
#ifndef SQLITE_OMIT_AUTOVACUUM
    if( pPage->pBt->autoVacuum ){
      int rc2 = SQLITE_OK;
      /* The cell may contain a pointer to an overflow page. If so, write
      ** the entry for the overflow page into the pointer map.
      */
      ptrmapPutOvflPtr(pPage, pPage, pCell, &rc2);
      if( rc2 ) return rc2;
    }
#endif
  }
  return SQLITE_OK;
}

/*
** The following parameters determine how many adjacent pages get involved
** in a balancing operation.  NN is the number of neighbors on either side
** of the page that participate in the balancing operation.  NB is the
** total number of pages that participate, including the target page and
** NN neighbors on either side.
**
** The minimum value of NN is 1 (of course).  Increasing NN above 1
** (to 2 or 3) gives a modest improvement in SELECT and DELETE performance
** in exchange for a larger degradation in INSERT and UPDATE performance.
** The value of NN appears to give the best results overall.
**
** (Later:) The description above makes it seem as if these values are
** tunable - as if you could change them and recompile and it would all work.
** But that is unlikely.  NB has been 3 since the inception of SQLite and
** we have never tested any other value.
*/
#define NN 1             /* Number of neighbors on either side of pPage */
#define NB 3             /* (NN*2+1): Total pages involved in the balance */

/*
** A CellArray object contains a cache of pointers and sizes for a
** consecutive sequence of cells that might be held on multiple pages.
**
** The cells in this array are the divider cell or cells from the pParent
** page plus up to three child pages.  There are a total of nCell cells.
**
** pRef is a pointer to one of the pages that contributes cells.  This is
** used to access information such as MemPage.intKey and MemPage.pBt->pageSize
** which should be common to all pages that contribute cells to this array.
**
** apCell[] and szCell[] hold, respectively, pointers to the start of each
** cell and the size of each cell.  Some of the apCell[] pointers might refer
** to overflow cells.  In other words, some apCel[] pointers might not point
** to content area of the pages.
**
** A szCell[] of zero means the size of that cell has not yet been computed.
**
** The cells come from as many as four different pages:
**
**             -----------
**             | Parent  |
**             -----------
**            /     |     \
**           /      |      \
**  ---------   ---------   ---------
**  |Child-1|   |Child-2|   |Child-3|
**  ---------   ---------   ---------
**
** The order of cells is in the array is for an index btree is:
**
**       1.  All cells from Child-1 in order
**       2.  The first divider cell from Parent
**       3.  All cells from Child-2 in order
**       4.  The second divider cell from Parent
**       5.  All cells from Child-3 in order
**
** For a table-btree (with rowids) the items 2 and 4 are empty because
** content exists only in leaves and there are no divider cells.
**
** For an index btree, the apEnd[] array holds pointer to the end of page
** for Child-1, the Parent, Child-2, the Parent (again), and Child-3,
** respectively. The ixNx[] array holds the number of cells contained in
** each of these 5 stages, and all stages to the left.  Hence:
**
**    ixNx[0] = Number of cells in Child-1.
**    ixNx[1] = Number of cells in Child-1 plus 1 for first divider.
**    ixNx[2] = Number of cells in Child-1 and Child-2 + 1 for 1st divider.
**    ixNx[3] = Number of cells in Child-1 and Child-2 + both divider cells
**    ixNx[4] = Total number of cells.
**
** For a table-btree, the concept is similar, except only apEnd[0]..apEnd[2]
** are used and they point to the leaf pages only, and the ixNx value are:
**
**    ixNx[0] = Number of cells in Child-1.
**    ixNx[1] = Number of cells in Child-1 and Child-2.
**    ixNx[2] = Total number of cells.
**
** Sometimes when deleting, a child page can have zero cells.  In those
** cases, ixNx[] entries with higher indexes, and the corresponding apEnd[]
** entries, shift down.  The end result is that each ixNx[] entry should
** be larger than the previous
*/
typedef struct CellArray CellArray;
struct CellArray {
  int nCell;              /* Number of cells in apCell[] */
  MemPage *pRef;          /* Reference page */
  u8 **apCell;            /* All cells begin balanced */
  u16 *szCell;            /* Local size of all cells in apCell[] */
  u8 *apEnd[NB*2];        /* MemPage.aDataEnd values */
  int ixNx[NB*2];         /* Index of at which we move to the next apEnd[] */
};

/*
** Make sure the cell sizes at idx, idx+1, ..., idx+N-1 have been
** computed.
*/
static void populateCellCache(CellArray *p, int idx, int N){
  MemPage *pRef = p->pRef;
  u16 *szCell = p->szCell;
  assert( idx>=0 && idx+N<=p->nCell );
  while( N>0 ){
    assert( p->apCell[idx]!=0 );
    if( szCell[idx]==0 ){
      szCell[idx] = pRef->xCellSize(pRef, p->apCell[idx]);
    }else{
      assert( CORRUPT_DB ||
              szCell[idx]==pRef->xCellSize(pRef, p->apCell[idx]) );
    }
    idx++;
    N--;
  }
}

/*
** Return the size of the Nth element of the cell array
*/
static SQLITE_NOINLINE u16 computeCellSize(CellArray *p, int N){
  assert( N>=0 && N<p->nCell );
  assert( p->szCell[N]==0 );
  p->szCell[N] = p->pRef->xCellSize(p->pRef, p->apCell[N]);
  return p->szCell[N];
}
static u16 cachedCellSize(CellArray *p, int N){
  assert( N>=0 && N<p->nCell );
  if( p->szCell[N] ) return p->szCell[N];
  return computeCellSize(p, N);
}

/*
** Array apCell[] contains pointers to nCell b-tree page cells. The 
** szCell[] array contains the size in bytes of each cell. This function
** replaces the current contents of page pPg with the contents of the cell
** array.
**
** Some of the cells in apCell[] may currently be stored in pPg. This
** function works around problems caused by this by making a copy of any 
** such cells before overwriting the page data.
**
** The MemPage.nFree field is invalidated by this function. It is the 
** responsibility of the caller to set it correctly.
*/
static int rebuildPage(
  CellArray *pCArray,             /* Content to be added to page pPg */
  int iFirst,                     /* First cell in pCArray to use */
  int nCell,                      /* Final number of cells on page */
  MemPage *pPg                    /* The page to be reconstructed */
){
  const int hdr = pPg->hdrOffset;          /* Offset of header on pPg */
  u8 * const aData = pPg->aData;           /* Pointer to data for pPg */
  const int usableSize = pPg->pBt->usableSize;
  u8 * const pEnd = &aData[usableSize];
  int i = iFirst;                 /* Which cell to copy from pCArray*/
  u32 j;                          /* Start of cell content area */
  int iEnd = i+nCell;             /* Loop terminator */
  u8 *pCellptr = pPg->aCellIdx;
  u8 *pTmp = sqlite3PagerTempSpace(pPg->pBt->pPager);
  u8 *pData;
  int k;                          /* Current slot in pCArray->apEnd[] */
  u8 *pSrcEnd;                    /* Current pCArray->apEnd[k] value */

  assert( i<iEnd );
  j = get2byte(&aData[hdr+5]);
  if( j>(u32)usableSize ){ j = 0; }
  memcpy(&pTmp[j], &aData[j], usableSize - j);

  for(k=0; pCArray->ixNx[k]<=i && ALWAYS(k<NB*2); k++){}
  pSrcEnd = pCArray->apEnd[k];

  pData = pEnd;
  while( 1/*exit by break*/ ){
    u8 *pCell = pCArray->apCell[i];
    u16 sz = pCArray->szCell[i];
    assert( sz>0 );
    if( SQLITE_WITHIN(pCell,aData+j,pEnd) ){
      if( ((uptr)(pCell+sz))>(uptr)pEnd ) return SQLITE_CORRUPT_BKPT;
      pCell = &pTmp[pCell - aData];
    }else if( (uptr)(pCell+sz)>(uptr)pSrcEnd
           && (uptr)(pCell)<(uptr)pSrcEnd
    ){
      return SQLITE_CORRUPT_BKPT;
    }

    pData -= sz;
    put2byte(pCellptr, (pData - aData));
    pCellptr += 2;
    if( pData < pCellptr ) return SQLITE_CORRUPT_BKPT;
    memmove(pData, pCell, sz);
    assert( sz==pPg->xCellSize(pPg, pCell) || CORRUPT_DB );
    i++;
    if( i>=iEnd ) break;
    if( pCArray->ixNx[k]<=i ){
      k++;
      pSrcEnd = pCArray->apEnd[k];
    }
  }

  /* The pPg->nFree field is now set incorrectly. The caller will fix it. */
  pPg->nCell = nCell;
  pPg->nOverflow = 0;

  put2byte(&aData[hdr+1], 0);
  put2byte(&aData[hdr+3], pPg->nCell);
  put2byte(&aData[hdr+5], pData - aData);
  aData[hdr+7] = 0x00;
  return SQLITE_OK;
}

/*
** The pCArray objects contains pointers to b-tree cells and the cell sizes.
** This function attempts to add the cells stored in the array to page pPg.
** If it cannot (because the page needs to be defragmented before the cells
** will fit), non-zero is returned. Otherwise, if the cells are added
** successfully, zero is returned.
**
** Argument pCellptr points to the first entry in the cell-pointer array
** (part of page pPg) to populate. After cell apCell[0] is written to the
** page body, a 16-bit offset is written to pCellptr. And so on, for each
** cell in the array. It is the responsibility of the caller to ensure
** that it is safe to overwrite this part of the cell-pointer array.
**
** When this function is called, *ppData points to the start of the 
** content area on page pPg. If the size of the content area is extended,
** *ppData is updated to point to the new start of the content area
** before returning.
**
** Finally, argument pBegin points to the byte immediately following the
** end of the space required by this page for the cell-pointer area (for
** all cells - not just those inserted by the current call). If the content
** area must be extended to before this point in order to accomodate all
** cells in apCell[], then the cells do not fit and non-zero is returned.
*/
static int pageInsertArray(
  MemPage *pPg,                   /* Page to add cells to */
  u8 *pBegin,                     /* End of cell-pointer array */
  u8 **ppData,                    /* IN/OUT: Page content-area pointer */
  u8 *pCellptr,                   /* Pointer to cell-pointer area */
  int iFirst,                     /* Index of first cell to add */
  int nCell,                      /* Number of cells to add to pPg */
  CellArray *pCArray              /* Array of cells */
){
  int i = iFirst;                 /* Loop counter - cell index to insert */
  u8 *aData = pPg->aData;         /* Complete page */
  u8 *pData = *ppData;            /* Content area.  A subset of aData[] */
  int iEnd = iFirst + nCell;      /* End of loop. One past last cell to ins */
  int k;                          /* Current slot in pCArray->apEnd[] */
  u8 *pEnd;                       /* Maximum extent of cell data */
  assert( CORRUPT_DB || pPg->hdrOffset==0 );    /* Never called on page 1 */
  if( iEnd<=iFirst ) return 0;
  for(k=0; pCArray->ixNx[k]<=i && ALWAYS(k<NB*2); k++){}
  pEnd = pCArray->apEnd[k];
  while( 1 /*Exit by break*/ ){
    int sz, rc;
    u8 *pSlot;
    assert( pCArray->szCell[i]!=0 );
    sz = pCArray->szCell[i];
    if( (aData[1]==0 && aData[2]==0) || (pSlot = pageFindSlot(pPg,sz,&rc))==0 ){
      if( (pData - pBegin)<sz ) return 1;
      pData -= sz;
      pSlot = pData;
    }
    /* pSlot and pCArray->apCell[i] will never overlap on a well-formed
    ** database.  But they might for a corrupt database.  Hence use memmove()
    ** since memcpy() sends SIGABORT with overlapping buffers on OpenBSD */
    assert( (pSlot+sz)<=pCArray->apCell[i]
         || pSlot>=(pCArray->apCell[i]+sz)
         || CORRUPT_DB );
    if( (uptr)(pCArray->apCell[i]+sz)>(uptr)pEnd
     && (uptr)(pCArray->apCell[i])<(uptr)pEnd
    ){
      assert( CORRUPT_DB );
      (void)SQLITE_CORRUPT_BKPT;
      return 1;
    }
    memmove(pSlot, pCArray->apCell[i], sz);
    put2byte(pCellptr, (pSlot - aData));
    pCellptr += 2;
    i++;
    if( i>=iEnd ) break;
    if( pCArray->ixNx[k]<=i ){
      k++;
      pEnd = pCArray->apEnd[k];
    }
  }
  *ppData = pData;
  return 0;
}

/*
** The pCArray object contains pointers to b-tree cells and their sizes.
**
** This function adds the space associated with each cell in the array
** that is currently stored within the body of pPg to the pPg free-list.
** The cell-pointers and other fields of the page are not updated.
**
** This function returns the total number of cells added to the free-list.
*/
static int pageFreeArray(
  MemPage *pPg,                   /* Page to edit */
  int iFirst,                     /* First cell to delete */
  int nCell,                      /* Cells to delete */
  CellArray *pCArray              /* Array of cells */
){
  u8 * const aData = pPg->aData;
  u8 * const pEnd = &aData[pPg->pBt->usableSize];
  u8 * const pStart = &aData[pPg->hdrOffset + 8 + pPg->childPtrSize];
  int nRet = 0;
  int i;
  int iEnd = iFirst + nCell;
  u8 *pFree = 0;                  /* \__ Parameters for pending call to */
  int szFree = 0;                 /* /   freeSpace()                    */

  for(i=iFirst; i<iEnd; i++){
    u8 *pCell = pCArray->apCell[i];
    if( SQLITE_WITHIN(pCell, pStart, pEnd) ){
      int sz;
      /* No need to use cachedCellSize() here.  The sizes of all cells that
      ** are to be freed have already been computing while deciding which
      ** cells need freeing */
      sz = pCArray->szCell[i];  assert( sz>0 );
      if( pFree!=(pCell + sz) ){
        if( pFree ){
          assert( pFree>aData && (pFree - aData)<65536 );
          freeSpace(pPg, (u16)(pFree - aData), szFree);
        }
        pFree = pCell;
        szFree = sz;
        if( pFree+sz>pEnd ){
          return 0;
        }
      }else{
        /* The current cell is adjacent to and before the pFree cell.
        ** Combine the two regions into one to reduce the number of calls
        ** to freeSpace(). */
        pFree = pCell;
        szFree += sz;
      }
      nRet++;
    }
  }
  if( pFree ){
    assert( pFree>aData && (pFree - aData)<65536 );
    freeSpace(pPg, (u16)(pFree - aData), szFree);
  }
  return nRet;
}

/*
** pCArray contains pointers to and sizes of all cells in the page being
** balanced.  The current page, pPg, has pPg->nCell cells starting with
** pCArray->apCell[iOld].  After balancing, this page should hold nNew cells
** starting at apCell[iNew].
**
** This routine makes the necessary adjustments to pPg so that it contains
** the correct cells after being balanced.
**
** The pPg->nFree field is invalid when this function returns. It is the
** responsibility of the caller to set it correctly.
*/
static int editPage(
  MemPage *pPg,                   /* Edit this page */
  int iOld,                       /* Index of first cell currently on page */
  int iNew,                       /* Index of new first cell on page */
  int nNew,                       /* Final number of cells on page */
  CellArray *pCArray              /* Array of cells and sizes */
){
  u8 * const aData = pPg->aData;
  const int hdr = pPg->hdrOffset;
  u8 *pBegin = &pPg->aCellIdx[nNew * 2];
  int nCell = pPg->nCell;       /* Cells stored on pPg */
  u8 *pData;
  u8 *pCellptr;
  int i;
  int iOldEnd = iOld + pPg->nCell + pPg->nOverflow;
  int iNewEnd = iNew + nNew;

#ifdef SQLITE_DEBUG
  u8 *pTmp = sqlite3PagerTempSpace(pPg->pBt->pPager);
  memcpy(pTmp, aData, pPg->pBt->usableSize);
#endif

  /* Remove cells from the start and end of the page */
  assert( nCell>=0 );
  if( iOld<iNew ){
    int nShift = pageFreeArray(pPg, iOld, iNew-iOld, pCArray);
    if( NEVER(nShift>nCell) ) return SQLITE_CORRUPT_BKPT;
    memmove(pPg->aCellIdx, &pPg->aCellIdx[nShift*2], nCell*2);
    nCell -= nShift;
  }
  if( iNewEnd < iOldEnd ){
    int nTail = pageFreeArray(pPg, iNewEnd, iOldEnd - iNewEnd, pCArray);
    assert( nCell>=nTail );
    nCell -= nTail;
  }

  pData = &aData[get2byteNotZero(&aData[hdr+5])];
  if( pData<pBegin ) goto editpage_fail;
  if( pData>pPg->aDataEnd ) goto editpage_fail;

  /* Add cells to the start of the page */
  if( iNew<iOld ){
    int nAdd = MIN(nNew,iOld-iNew);
    assert( (iOld-iNew)<nNew || nCell==0 || CORRUPT_DB );
    assert( nAdd>=0 );
    pCellptr = pPg->aCellIdx;
    memmove(&pCellptr[nAdd*2], pCellptr, nCell*2);
    if( pageInsertArray(
          pPg, pBegin, &pData, pCellptr,
          iNew, nAdd, pCArray
    ) ) goto editpage_fail;
    nCell += nAdd;
  }

  /* Add any overflow cells */
  for(i=0; i<pPg->nOverflow; i++){
    int iCell = (iOld + pPg->aiOvfl[i]) - iNew;
    if( iCell>=0 && iCell<nNew ){
      pCellptr = &pPg->aCellIdx[iCell * 2];
      if( nCell>iCell ){
        memmove(&pCellptr[2], pCellptr, (nCell - iCell) * 2);
      }
      nCell++;
      cachedCellSize(pCArray, iCell+iNew);
      if( pageInsertArray(
            pPg, pBegin, &pData, pCellptr,
            iCell+iNew, 1, pCArray
      ) ) goto editpage_fail;
    }
  }

  /* Append cells to the end of the page */
  assert( nCell>=0 );
  pCellptr = &pPg->aCellIdx[nCell*2];
  if( pageInsertArray(
        pPg, pBegin, &pData, pCellptr,
        iNew+nCell, nNew-nCell, pCArray
  ) ) goto editpage_fail;

  pPg->nCell = nNew;
  pPg->nOverflow = 0;

  put2byte(&aData[hdr+3], pPg->nCell);
  put2byte(&aData[hdr+5], pData - aData);

#ifdef SQLITE_DEBUG
  for(i=0; i<nNew && !CORRUPT_DB; i++){
    u8 *pCell = pCArray->apCell[i+iNew];
    int iOff = get2byteAligned(&pPg->aCellIdx[i*2]);
    if( SQLITE_WITHIN(pCell, aData, &aData[pPg->pBt->usableSize]) ){
      pCell = &pTmp[pCell - aData];
    }
    assert( 0==memcmp(pCell, &aData[iOff],
            pCArray->pRef->xCellSize(pCArray->pRef, pCArray->apCell[i+iNew])) );
  }
#endif

  return SQLITE_OK;
 editpage_fail:
  /* Unable to edit this page. Rebuild it from scratch instead. */
  populateCellCache(pCArray, iNew, nNew);
  return rebuildPage(pCArray, iNew, nNew, pPg);
}


#ifndef SQLITE_OMIT_QUICKBALANCE
/*
** This version of balance() handles the common special case where
** a new entry is being inserted on the extreme right-end of the
** tree, in other words, when the new entry will become the largest
** entry in the tree.
**
** Instead of trying to balance the 3 right-most leaf pages, just add
** a new page to the right-hand side and put the one new entry in
** that page.  This leaves the right side of the tree somewhat
** unbalanced.  But odds are that we will be inserting new entries
** at the end soon afterwards so the nearly empty page will quickly
** fill up.  On average.
**
** pPage is the leaf page which is the right-most page in the tree.
** pParent is its parent.  pPage must have a single overflow entry
** which is also the right-most entry on the page.
**
** The pSpace buffer is used to store a temporary copy of the divider
** cell that will be inserted into pParent. Such a cell consists of a 4
** byte page number followed by a variable length integer. In other
** words, at most 13 bytes. Hence the pSpace buffer must be at
** least 13 bytes in size.
*/
static int balance_quick(MemPage *pParent, MemPage *pPage, u8 *pSpace){
  BtShared *const pBt = pPage->pBt;    /* B-Tree Database */
  MemPage *pNew;                       /* Newly allocated page */
  int rc;                              /* Return Code */
  Pgno pgnoNew;                        /* Page number of pNew */

  assert( sqlite3_mutex_held(pPage->pBt->mutex) );
  assert( sqlite3PagerIswriteable(pParent->pDbPage) );
  assert( pPage->nOverflow==1 );
  
  if( pPage->nCell==0 ) return SQLITE_CORRUPT_BKPT;  /* dbfuzz001.test */
  assert( pPage->nFree>=0 );
  assert( pParent->nFree>=0 );

  /* Allocate a new page. This page will become the right-sibling of 
  ** pPage. Make the parent page writable, so that the new divider cell
  ** may be inserted. If both these operations are successful, proceed.
  */
  rc = allocateBtreePage(pBt, &pNew, &pgnoNew, 0, 0);

  if( rc==SQLITE_OK ){

    u8 *pOut = &pSpace[4];
    u8 *pCell = pPage->apOvfl[0];
    u16 szCell = pPage->xCellSize(pPage, pCell);
    u8 *pStop;
    CellArray b;

    assert( sqlite3PagerIswriteable(pNew->pDbPage) );
    assert( CORRUPT_DB || pPage->aData[0]==(PTF_INTKEY|PTF_LEAFDATA|PTF_LEAF) );
    zeroPage(pNew, PTF_INTKEY|PTF_LEAFDATA|PTF_LEAF);
    b.nCell = 1;
    b.pRef = pPage;
    b.apCell = &pCell;
    b.szCell = &szCell;
    b.apEnd[0] = pPage->aDataEnd;
    b.ixNx[0] = 2;
    rc = rebuildPage(&b, 0, 1, pNew);
    if( NEVER(rc) ){
      releasePage(pNew);
      return rc;
    }
    pNew->nFree = pBt->usableSize - pNew->cellOffset - 2 - szCell;

    /* If this is an auto-vacuum database, update the pointer map
    ** with entries for the new page, and any pointer from the 
    ** cell on the page to an overflow page. If either of these
    ** operations fails, the return code is set, but the contents
    ** of the parent page are still manipulated by thh code below.
    ** That is Ok, at this point the parent page is guaranteed to
    ** be marked as dirty. Returning an error code will cause a
    ** rollback, undoing any changes made to the parent page.
    */
    if( ISAUTOVACUUM(pBt) ){
      ptrmapPut(pBt, pgnoNew, PTRMAP_BTREE, pParent->pgno, &rc);
      if( szCell>pNew->minLocal ){
        ptrmapPutOvflPtr(pNew, pNew, pCell, &rc);
      }
    }
  
    /* Create a divider cell to insert into pParent. The divider cell
    ** consists of a 4-byte page number (the page number of pPage) and
    ** a variable length key value (which must be the same value as the
    ** largest key on pPage).
    **
    ** To find the largest key value on pPage, first find the right-most 
    ** cell on pPage. The first two fields of this cell are the 
    ** record-length (a variable length integer at most 32-bits in size)
    ** and the key value (a variable length integer, may have any value).
    ** The first of the while(...) loops below skips over the record-length
    ** field. The second while(...) loop copies the key value from the
    ** cell on pPage into the pSpace buffer.
    */
    pCell = findCell(pPage, pPage->nCell-1);
    pStop = &pCell[9];
    while( (*(pCell++)&0x80) && pCell<pStop );
    pStop = &pCell[9];
    while( ((*(pOut++) = *(pCell++))&0x80) && pCell<pStop );

    /* Insert the new divider cell into pParent. */
    if( rc==SQLITE_OK ){
      rc = insertCell(pParent, pParent->nCell, pSpace, (int)(pOut-pSpace),
                      0, pPage->pgno);
    }

    /* Set the right-child pointer of pParent to point to the new page. */
    put4byte(&pParent->aData[pParent->hdrOffset+8], pgnoNew);
  
    /* Release the reference to the new page. */
    releasePage(pNew);
  }

  return rc;
}
#endif /* SQLITE_OMIT_QUICKBALANCE */

#if 0
/*
** This function does not contribute anything to the operation of SQLite.
** it is sometimes activated temporarily while debugging code responsible 
** for setting pointer-map entries.
*/
static int ptrmapCheckPages(MemPage **apPage, int nPage){
  int i, j;
  for(i=0; i<nPage; i++){
    Pgno n;
    u8 e;
    MemPage *pPage = apPage[i];
    BtShared *pBt = pPage->pBt;
    assert( pPage->isInit );

    for(j=0; j<pPage->nCell; j++){
      CellInfo info;
      u8 *z;
     
      z = findCell(pPage, j);
      pPage->xParseCell(pPage, z, &info);
      if( info.nLocal<info.nPayload ){
        Pgno ovfl = get4byte(&z[info.nSize-4]);
        ptrmapGet(pBt, ovfl, &e, &n);
        assert( n==pPage->pgno && e==PTRMAP_OVERFLOW1 );
      }
      if( !pPage->leaf ){
        Pgno child = get4byte(z);
        ptrmapGet(pBt, child, &e, &n);
        assert( n==pPage->pgno && e==PTRMAP_BTREE );
      }
    }
    if( !pPage->leaf ){
      Pgno child = get4byte(&pPage->aData[pPage->hdrOffset+8]);
      ptrmapGet(pBt, child, &e, &n);
      assert( n==pPage->pgno && e==PTRMAP_BTREE );
    }
  }
  return 1;
}
#endif

/*
** 本函数用于将存储于pFrom页的B树节点内容复制到pTo页，处理流程如下：
** 
** 指针映射更新规则：
**   (1) 若pFrom是非叶子页：
**       a. 更新所有子页的指针映射条目，将其父页指向pTo
**   (2) 若pFrom包含带有溢出页指针的单元格：
**       a. 更新对应指针映射条目，父页设置为pTo
**
** 特殊处理说明：
**   - 不复制pFrom当前携带的溢出单元格（MemPage.apOvfl[]数组中的条目）
**   - 返回前使用btreeInitPage()重新初始化pTo页
**
** 性能备注：
**   本函数性能要求不高，仅在以下不常调用的场景中使用：
**     (a) balance_shallower() 过程 
**     (b) balance_deeper() 过程
*/
static void copyNodeContent(MemPage *pFrom, MemPage *pTo, int *pRC){
  if( (*pRC)==SQLITE_OK ){
    BtShared * const pBt = pFrom->pBt;
    u8 * const aFrom = pFrom->aData;
    u8 * const aTo = pTo->aData;
    int const iFromHdr = pFrom->hdrOffset;
    int const iToHdr = ((pTo->pgno==1) ? 100 : 0);
    int rc;
    int iData;
  
  
    assert( pFrom->isInit );
    assert( pFrom->nFree>=iToHdr );
    assert( get2byte(&aFrom[iFromHdr+5]) <= (int)pBt->usableSize );
  
    // 拷贝Cell和slot数组
    iData = get2byte(&aFrom[iFromHdr+5]);
    memcpy(&aTo[iData], &aFrom[iData], pBt->usableSize-iData);
    memcpy(&aTo[iToHdr], &aFrom[iFromHdr], pFrom->cellOffset + 2*pFrom->nCell);

    pTo->isInit = 0;
    rc = btreeInitPage(pTo);
    if( rc==SQLITE_OK ) rc = btreeComputeFreeSpace(pTo);
    if( rc!=SQLITE_OK ){
      *pRC = rc;
      return;
    }
  
    // 更新子节点和溢出页的指针映射
    if( ISAUTOVACUUM(pBt) ){
      *pRC = setChildPtrmaps(pTo);
    }
  }
}

/*
** 本函数用于对父节点(pParent)的第iParentIdx个子页（下称"目标页"）及其兄弟页进行单元格重分配，
** 使所有相关页的空闲空间趋于均衡。具体处理逻辑如下：
**
** 兄弟页选择规则：
**   (1) 通常选取目标页的左右各一个兄弟页参与平衡
**   (2) 若目标页是父节点的首/末子节点，则可能仅使用同一侧的两个兄弟页
**   (3) 当目标页为根页或根页直接子页时（兄弟页少于2个），所有可用兄弟页参与平衡
**
** 动态调整机制：
**   - 可能增减1-2个兄弟页参与平衡，以维持页面接近满载但不过载的状态
**
** 数据存储保障：
**   - 即使目标页存在未存入MemPage.aData[]的单元格（过载状态时发生）
**   - 确保平衡完成后，所有分配到目标页及其兄弟页的单元格均完整存储于MemPage.aData[]
**
** 父页维护要求：
**   - 平衡操作可能导致父页(pParent)插入/删除单元格，引发父页过载或欠载
**   - 调用方需负责触发后续平衡操作修复父页状态（参见balance()函数）
**
** 错误处理规范：
**   - 若本函数执行失败，可能导致数据库损坏
**   - 必须执行数据库回滚操作以保证数据安全
**
** 溢出缓冲区说明：
**   - 参数aOvflSpace指向可容纳整页数据的缓冲区
**   - 当父页插入单元格发生过载时，用于临时存储溢出的单元格
**   - 安全容量保障：最多插入4个分隔单元格，每个大小恒小于页尺寸的1/4
**   - 特殊错误码：aOvflSpace为空时返回SQLITE_NOMEM
*/
static int balance_nonroot(
  MemPage *pParent,               /* Parent page of siblings being balanced */
  int iParentIdx,                 /* Index of "the page" in pParent */ // 对pParent的第iParentIdx个子节点进行Cell重新分配
  u8 *aOvflSpace,                 /* page-size bytes of space for parent ovfl */
  int isRoot,                     // pParent 是否是根页
  int bBulk                       /* True if this call is part of a bulk load */
){
  BtShared *pBt;               /* The whole database */
  int nMaxCells = 0;           /* Allocated size of apCell, szCell, aFrom. */
  int nNew = 0;                /* Number of pages in apNew[] */
  int nOld;                    /* Number of pages in apOld[] */
  int i, j, k;                 /* Loop counters */
  int nxDiv;                   /* Next divider slot in pParent->aCell[] */
  int rc = SQLITE_OK;          /* The return code */
  u16 leafCorrection;          /* 4 if pPage is a leaf.  0 if not */
  int leafData;                /* True if pPage is a leaf of a LEAFDATA tree */
  int usableSpace;             /* Bytes in pPage beyond the header */
  int pageFlags;               /* Value of pPage->aData[0] */
  int iSpace1 = 0;             /* First unused byte of aSpace1[] */
  int iOvflSpace = 0;          /* First unused byte of aOvflSpace[] */
  int szScratch;               /* Size of scratch memory requested */
  MemPage *apOld[NB];          // pPage 和最多两个兄弟节点
  MemPage *apNew[NB+2];        // 平衡操作后的pPage及最多NB个兄弟页
  u8 *pRight;                  /* Location in parent of right-sibling pointer */
  u8 *apDiv[NB-1];             /* Divider cells in pParent */
  int cntNew[NB+2];            /* Index in b.paCell[] of cell after i-th page */
  int cntOld[NB+2];            /* Old index in b.apCell[] */
  int szNew[NB+2];             /* Combined size of cells placed on i-th page */
  u8 *aSpace1;                 /* Space for copies of dividers cells */
  Pgno pgno;                   /* Temp var to store a page number in */
  u8 abDone[NB+2];             /* True after i'th new page is populated */
  Pgno aPgno[NB+2];            /* Page numbers of new pages before shuffling */
  CellArray b;                 /* Parsed information on cells being balanced */

  memset(abDone, 0, sizeof(abDone));
  memset(&b, 0, sizeof(b));
  pBt = pParent->pBt;
  assert( sqlite3_mutex_held(pBt->mutex) );
  assert( sqlite3PagerIswriteable(pParent->pDbPage) );

  /* 当前状态下，pParent至多包含一个溢出单元（overflow cell）。若存在
  ** 该溢出单元，其索引（index）必须为iParentIdx。该场景出现在本函数
  ** 被sqlite3BtreeDelete()间接调用时。*/
  assert( pParent->nOverflow==0 || pParent->nOverflow==1 );
  assert( pParent->nOverflow==0 || pParent->aiOvfl[0]==iParentIdx );

  assert( pParent->nFree>=0 );

  /* 
  ** 定位需要平衡的兄弟页(B-tree sibling pages)，并确定父页(pParent)中
  ** 用于分割这些兄弟页的单元(cells)。系统会尝试在pPage两侧各查找NN个兄弟，
  ** 但当某一侧兄弟数量不足NN时，将从另一侧获取更多兄弟。若pParent的子节点数
  ** 不超过NB，则直接采用其所有子节点。
  **
  ** 本循环同时移除父页中的分割单元(divider cells)。通过此操作，函数后续逻辑
  ** 无需处理父页中的溢出单元(overflow cells)——若存在此类单元，其已在当前
  ** 操作中被提前移除。
  */
  i = pParent->nOverflow + pParent->nCell;
  if( i<2 ){ // Cell 总数小于 2
    nxDiv = 0;
  }else{
    assert( bBulk==0 || bBulk==1 );
    if( iParentIdx==0 ){ // 当前节点是父节点的第一个子节点        
      nxDiv = 0;
    }else if( iParentIdx==i ){  // 当前节点是父节点的最后一个子节点
      nxDiv = i-2+bBulk;
    }else{ // 中间位置
      nxDiv = iParentIdx-1;
    }
    i = 2-bBulk; // 后续操作的最小键值数（批量模式为1，正常模式为2）
  }
  nOld = i+1;
  //取范围内的最后的孩子节点页号
  if( (i+nxDiv-pParent->nOverflow)==pParent->nCell ){
    pRight = &pParent->aData[pParent->hdrOffset+8];
  }else{
    pRight = findCell(pParent, i+nxDiv-pParent->nOverflow);
  }
  pgno = get4byte(pRight);
  while( 1 ){
    if( rc==SQLITE_OK ){
      rc = getAndInitPage(pBt, pgno, &apOld[i], 0, 0);
    }
    if( rc ){
      memset(apOld, 0, (i+1)*sizeof(MemPage*));
      goto balance_cleanup;
    }
    if( apOld[i]->nFree<0 ){
      rc = btreeComputeFreeSpace(apOld[i]);
      if( rc ){
        memset(apOld, 0, (i)*sizeof(MemPage*));
        goto balance_cleanup;
      }
    }
    nMaxCells += apOld[i]->nCell + ArraySize(pParent->apOvfl);
    if( (i--)==0 ) break;

    if( pParent->nOverflow && i+nxDiv==pParent->aiOvfl[0] ){
      apDiv[i] = pParent->apOvfl[0];
      pgno = get4byte(apDiv[i]);
      szNew[i] = pParent->xCellSize(pParent, apDiv[i]);
      pParent->nOverflow = 0;
    }else{
      apDiv[i] = findCell(pParent, i+nxDiv-pParent->nOverflow);
      pgno = get4byte(apDiv[i]);
      szNew[i] = pParent->xCellSize(pParent, apDiv[i]);

      /* 从父页面移除指定单元格。即使单元格已被移除，apDiv[i]仍指向父页内的原单元格位置。此操作的安全性在于：
      ** 1. 常规模式下：移除操作仅覆盖单元格前4字节
      ** 2. 本函数无需使用分隔单元格的前4字节
      ** 因此后续操作仍可安全使用该指针
      **
      ** 例外情况 - 安全删除模式：
      ** a) dropCell()会将整个单元格清零
      ** b) 此时需临时将单元格复制到aOvflSpace[]缓冲区
      ** c) 待aSpace[]缓冲区分配后立即重新拷贝出数据
      */
      if( pBt->btsFlags & BTS_FAST_SECURE ){
        int iOff;

        /* If the following if() condition is not true, the db is corrupted.
        ** The call to dropCell() below will detect this.  */
        iOff = SQLITE_PTR_TO_INT(apDiv[i]) - SQLITE_PTR_TO_INT(pParent->aData);
        if( (iOff+szNew[i])<=(int)pBt->usableSize ){
          memcpy(&aOvflSpace[iOff], apDiv[i], szNew[i]);
          apDiv[i] = &aOvflSpace[apDiv[i]-pParent->aData];
        }
      }
      dropCell(pParent, i+nxDiv-pParent->nOverflow, szNew[i], &rc);
    }
  }

  /* Make nMaxCells a multiple of 4 in order to preserve 8-byte
  ** alignment */
  nMaxCells = (nMaxCells + 3)&~3;

  /*
  ** Allocate space for memory structures
  */
  szScratch =
       nMaxCells*sizeof(u8*)                       /* b.apCell */
     + nMaxCells*sizeof(u16)                       /* b.szCell */
     + pBt->pageSize;                              /* aSpace1 */

  assert( szScratch<=7*(int)pBt->pageSize );
  b.apCell = sqlite3StackAllocRaw(0, szScratch );
  if( b.apCell==0 ){
    rc = SQLITE_NOMEM_BKPT;
    goto balance_cleanup;
  }
  b.szCell = (u16*)&b.apCell[nMaxCells];
  aSpace1 = (u8*)&b.szCell[nMaxCells];
  assert( EIGHT_BYTE_ALIGNMENT(aSpace1) );

  /* 将兄弟页上的所有单元指针及分割单元(divider cells)加载到
  ** 本地b.apCell[]数组。分割单元副本存储于aSpace1[]分配的空间，
  ** 这些分割单元此前已从pParent中移除。
  **
  ** 若兄弟页为叶子页(leaf pages)，在将分割单元复制到aSpace1[]前，
  ** 会剥离其中的子指针(child pointers)。如此处理后，b.apCell[]中
  ** 所有单元均不含子指针。若兄弟页非叶子页，则b.apCell[]中的单元
  ** 均保留子指针。无论何种情况，b.apCell[]内的单元均保持结构一致性。
  **
  ** leafCorrection: pPage为叶子页时取值4，非叶子页时取值0
  ** leafData: 当pPage存储键值+数据而pParent仅存键时取1 */
  b.pRef = apOld[0];
  leafCorrection = b.pRef->leaf*4;
  leafData = b.pRef->intKeyLeaf;
  for(i=0; i<nOld; i++){
    MemPage *pOld = apOld[i];
    int limit = pOld->nCell;
    u8 *aData = pOld->aData;
    u16 maskPage = pOld->maskPage;
    u8 *piCell = aData + pOld->cellOffset;
    u8 *piEnd;
    VVA_ONLY( int nCellAtStart = b.nCell; )

    /* Verify that all sibling pages are of the same "type" (table-leaf,
    ** table-interior, index-leaf, or index-interior).
    */
    if( pOld->aData[0]!=apOld[0]->aData[0] ){
      rc = SQLITE_CORRUPT_BKPT;
      goto balance_cleanup;
    }

    /* 将pOld中所有单元的指针加载至b.apCell[]数组。若pOld包含
    ** 溢出单元(overflow cells)，需将其按正确顺序插入b.apCell[]数组。
    ** 注意：当存在多个溢出单元时，这些单元必定是连续且相邻的。此不变式
    ** 成立的原因在于，多个溢出单元仅在前次平衡操作向父节点插入分割单元时
    ** 产生，而分割单元本身总是相邻且按序插入。在溢出单元插入循环中存在
    ** 标有"NOTE 1"的assert()（断言）用于验证此不变式。
    ** 此操作为必要的前置操作。一旦平衡操作启动，B树页的单元偏移区将被
    ** 覆写，若未提前保存所有单元的指针，后续将无法定位这些单元。 */
    memset(&b.szCell[b.nCell], 0, sizeof(b.szCell[0])*(limit+pOld->nOverflow));
    if( pOld->nOverflow>0 ){
      if( NEVER(limit<pOld->aiOvfl[0]) ){
        rc = SQLITE_CORRUPT_BKPT;
        goto balance_cleanup;
      }
      limit = pOld->aiOvfl[0];
      for(j=0; j<limit; j++){
        b.apCell[b.nCell] = aData + (maskPage & get2byteAligned(piCell));
        piCell += 2;
        b.nCell++;
      }
      for(k=0; k<pOld->nOverflow; k++){
        assert( k==0 || pOld->aiOvfl[k-1]+1==pOld->aiOvfl[k] );/* NOTE 1 */
        b.apCell[b.nCell] = pOld->apOvfl[k];
        b.nCell++;
      }
    }
    piEnd = aData + pOld->cellOffset + 2*pOld->nCell;
    while( piCell<piEnd ){
      assert( b.nCell<nMaxCells );
      b.apCell[b.nCell] = aData + (maskPage & get2byteAligned(piCell));
      piCell += 2;
      b.nCell++;
    }
    assert( (b.nCell-nCellAtStart)==(pOld->nCell+pOld->nOverflow) );

    cntOld[i] = b.nCell;
    if( i<nOld-1 && !leafData){
      u16 sz = (u16)szNew[i];
      u8 *pTemp;
      assert( b.nCell<nMaxCells );
      b.szCell[b.nCell] = sz;
      pTemp = &aSpace1[iSpace1];
      iSpace1 += sz;
      assert( sz<=pBt->maxLocal+23 );
      assert( iSpace1 <= (int)pBt->pageSize );
      memcpy(pTemp, apDiv[i], sz);
      b.apCell[b.nCell] = pTemp+leafCorrection;
      assert( leafCorrection==0 || leafCorrection==4 );
      b.szCell[b.nCell] = b.szCell[b.nCell] - leafCorrection;
      if( !pOld->leaf ){
        assert( leafCorrection==0 );
        assert( pOld->hdrOffset==0 || CORRUPT_DB );
        /* The right pointer of the child page pOld becomes the left
        ** pointer of the divider cell */
        memcpy(b.apCell[b.nCell], &pOld->aData[8], 4);
      }else{
        assert( leafCorrection==4 );
        while( b.szCell[b.nCell]<4 ){
          /* Do not allow any cells smaller than 4 bytes. If a smaller cell
          ** does exist, pad it with 0x00 bytes. */
          assert( b.szCell[b.nCell]==3 || CORRUPT_DB );
          assert( b.apCell[b.nCell]==&aSpace1[iSpace1-3] || CORRUPT_DB );
          aSpace1[iSpace1++] = 0x00;
          b.szCell[b.nCell]++;
        }
      }
      b.nCell++;
    }
  }

  /* 计算存储所有b.nCell个单元所需的页数，将此数值存入变量k。
  ** 同时计算szNew[]数组（各元素表示第i个兄弟页的单元总大小）与
  ** cntNew[]数组（各元素指示b.apCell[]中分隔第i页与第i+1页的单元索引），
  ** 最终cntNew[k]应等于b.nCell。
  ** 本代码块计算的变量：
  ** k: 兄弟页总数
  ** szNew[i]: 第i个兄弟页已用空间
  ** cntNew[i]: 分隔第i页的起始单元在b.apCell[]与b.szCell[]中的索引位置
  ** usableSpace: 每个兄弟页的可用空间字节数
  */
  usableSpace = pBt->usableSize - 12 + leafCorrection;
  for(i=k=0; i<nOld; i++, k++){
    MemPage *p = apOld[i];
    b.apEnd[k] = p->aDataEnd;
    b.ixNx[k] = cntOld[i];
    if( k && b.ixNx[k]==b.ixNx[k-1] ){
      k--;  /* Omit b.ixNx[] entry for child pages with no cells */
    }
    if( !leafData ){
      k++;
      b.apEnd[k] = pParent->aDataEnd;
      b.ixNx[k] = cntOld[i]+1;
    }
    assert( p->nFree>=0 );
    szNew[i] = usableSpace - p->nFree;
    for(j=0; j<p->nOverflow; j++){
      szNew[i] += 2 + p->xCellSize(p, p->apOvfl[j]);
    }
    cntNew[i] = cntOld[i];
  }
  k = nOld;
  for(i=0; i<k; i++){
    int sz;
    while( szNew[i]>usableSpace ){
      if( i+1>=k ){
        k = i+2;
        if( k>NB+2 ){ rc = SQLITE_CORRUPT_BKPT; goto balance_cleanup; }
        szNew[k-1] = 0;
        cntNew[k-1] = b.nCell;
      }
      sz = 2 + cachedCellSize(&b, cntNew[i]-1);
      szNew[i] -= sz;
      if( !leafData ){
        if( cntNew[i]<b.nCell ){
          sz = 2 + cachedCellSize(&b, cntNew[i]);
        }else{
          sz = 0;
        }
      }
      szNew[i+1] += sz;
      cntNew[i]--;
    }
    while( cntNew[i]<b.nCell ){
      sz = 2 + cachedCellSize(&b, cntNew[i]);
      if( szNew[i]+sz>usableSpace ) break;
      szNew[i] += sz;
      cntNew[i]++;
      if( !leafData ){
        if( cntNew[i]<b.nCell ){
          sz = 2 + cachedCellSize(&b, cntNew[i]);
        }else{
          sz = 0;
        }
      }
      szNew[i+1] -= sz;
    }
    if( cntNew[i]>=b.nCell ){
      k = i+1;
    }else if( cntNew[i] <= (i>0 ? cntNew[i-1] : 0) ){
      rc = SQLITE_CORRUPT_BKPT;
      goto balance_cleanup;
    }
  }

  /* 由前序代码块计算的分页策略会偏向键值较小的左侧兄弟页。这些左侧兄弟页
  ** 通常接近满载，而最右侧兄弟页可能接近空置。接下来的代码块将尝试调整兄弟页的
  ** 数据分布策略，以获取更均衡的空间分配。
  ** 注：此调整不仅是优化操作。前述分页策略可能严重失衡至非法状态。例如，
  ** 最右侧兄弟页可能完全为空。此调整步骤是必须执行的强制性操作。 */
  for(i=k-1; i>0; i--){
    int szRight = szNew[i];  /* Size of sibling on the right */
    int szLeft = szNew[i-1]; /* Size of sibling on the left */
    int r;              /* Index of right-most cell in left sibling */
    int d;              /* Index of first cell to the left of right sibling */

    r = cntNew[i-1] - 1;
    d = r + 1 - leafData;
    (void)cachedCellSize(&b, d);
    do{
      int szR, szD;
      assert( d<nMaxCells );
      assert( r<nMaxCells );
      szR = cachedCellSize(&b, r);
      szD = b.szCell[d];
      if( szRight!=0
       && (bBulk || szRight+szD+2 > szLeft-(szR+(i==k-1?0:2)))){
        break;
      }
      szRight += szD + 2;
      szLeft -= szR + 2;
      cntNew[i-1] = r;
      r--;
      d--;
    }while( r>=0 );
    szNew[i] = szRight;
    szNew[i-1] = szLeft;
    if( cntNew[i-1] <= (i>1 ? cntNew[i-2] : 0) ){
      rc = SQLITE_CORRUPT_BKPT;
      goto balance_cleanup;
    }
  }

  /* 完整性检查：对于非损坏的数据库文件，必须满足以下条件之一：
  **
  ** (1) 至少存在一个存储单元（即 cntNew > 0），或
  ** (2) pPage是虚拟根页。虚拟根页是指：当实际根页为页1时，
  ** 当前页作为该页的唯一子页存在。
  */
  assert( cntNew[0]>0 || (pParent->pgno==1 && pParent->nCell==0) || CORRUPT_DB);
  TRACE(("BALANCE: old: %d(nc=%d) %d(nc=%d) %d(nc=%d)\n",
    apOld[0]->pgno, apOld[0]->nCell,
    nOld>=2 ? apOld[1]->pgno : 0, nOld>=2 ? apOld[1]->nCell : 0,
    nOld>=3 ? apOld[2]->pgno : 0, nOld>=3 ? apOld[2]->nCell : 0
  ));

  /*
  ** Allocate k new pages.  Reuse old pages where possible.
  */
  pageFlags = apOld[0]->aData[0];
  for(i=0; i<k; i++){
    MemPage *pNew;
    if( i<nOld ){
      pNew = apNew[i] = apOld[i];
      apOld[i] = 0;
      rc = sqlite3PagerWrite(pNew->pDbPage);
      nNew++;
      if( sqlite3PagerPageRefcount(pNew->pDbPage)!=1+(i==(iParentIdx-nxDiv))
       && rc==SQLITE_OK
      ){
        rc = SQLITE_CORRUPT_BKPT;
      }
      if( rc ) goto balance_cleanup;
    }else{
      assert( i>0 );
      rc = allocateBtreePage(pBt, &pNew, &pgno, (bBulk ? 1 : pgno), 0);
      if( rc ) goto balance_cleanup;
      zeroPage(pNew, pageFlags);
      apNew[i] = pNew;
      nNew++;
      cntOld[i] = b.nCell;

      /* Set the pointer-map entry for the new sibling page. */
      if( ISAUTOVACUUM(pBt) ){
        ptrmapPut(pBt, pNew->pgno, PTRMAP_BTREE, pParent->pgno, &rc);
        if( rc!=SQLITE_OK ){
          goto balance_cleanup;
        }
      }
    }
  }

  /* 重新编排页码以确保新页面按升序排列。此操作优化磁盘文件的物理存储顺序，
  ** 使得表扫描更接近文件的线性遍历模式。进而帮助操作系统更高效地从磁盘加载数据页。
  **
  ** 虽然采用O(N²)时间复杂度排序算法，但由于N的实际值始终不超过NB+2
  ** (通常为5)，性能影响可忽略不计。
  **
  ** 当NB==3时，仅此优化即可使数据库在大规模插入/删除操作中获得约25%的性能提升。
  */
  for(i=0; i<nNew; i++){
    aPgno[i] = apNew[i]->pgno;
    assert( apNew[i]->pDbPage->flags & PGHDR_WRITEABLE );
    assert( apNew[i]->pDbPage->flags & PGHDR_DIRTY );
  }
  for(i=0; i<nNew-1; i++){
    int iB = i;
    for(j=i+1; j<nNew; j++){
      if( apNew[j]->pgno < apNew[iB]->pgno ) iB = j;
    }

    /* 若当前页apNew[i]的页码大于其后续所有apNew[i]项的页码，
    ** 则将其与后续项中页码最小的条目（已知该条目为apNew[iB]）进行交换。
    */
    if( iB!=i ){
      Pgno pgnoA = apNew[i]->pgno;
      Pgno pgnoB = apNew[iB]->pgno;
      Pgno pgnoTemp = (PENDING_BYTE/pBt->pageSize)+1;
      u16 fgA = apNew[i]->pDbPage->flags;
      u16 fgB = apNew[iB]->pDbPage->flags;
      sqlite3PagerRekey(apNew[i]->pDbPage, pgnoTemp, fgB);
      sqlite3PagerRekey(apNew[iB]->pDbPage, pgnoA, fgA);
      sqlite3PagerRekey(apNew[i]->pDbPage, pgnoB, fgB);
      apNew[i]->pgno = pgnoB;
      apNew[iB]->pgno = pgnoA;
    }
  }

  TRACE(("BALANCE: new: %d(%d nc=%d) %d(%d nc=%d) %d(%d nc=%d) "
         "%d(%d nc=%d) %d(%d nc=%d)\n",
    apNew[0]->pgno, szNew[0], cntNew[0],
    nNew>=2 ? apNew[1]->pgno : 0, nNew>=2 ? szNew[1] : 0,
    nNew>=2 ? cntNew[1] - cntNew[0] - !leafData : 0,
    nNew>=3 ? apNew[2]->pgno : 0, nNew>=3 ? szNew[2] : 0,
    nNew>=3 ? cntNew[2] - cntNew[1] - !leafData : 0,
    nNew>=4 ? apNew[3]->pgno : 0, nNew>=4 ? szNew[3] : 0,
    nNew>=4 ? cntNew[3] - cntNew[2] - !leafData : 0,
    nNew>=5 ? apNew[4]->pgno : 0, nNew>=5 ? szNew[4] : 0,
    nNew>=5 ? cntNew[4] - cntNew[3] - !leafData : 0
  ));

  assert( sqlite3PagerIswriteable(pParent->pDbPage) );
  assert( nNew>=1 && nNew<=ArraySize(apNew) );
  assert( apNew[nNew-1]!=0 );
  put4byte(pRight, apNew[nNew-1]->pgno);

  /* 若兄弟页非叶子页，则需确保最右侧新兄弟页的右子指针
  ** 被设置为原兄弟页中最右侧旧兄弟页的同名字段值。*/
  if( (pageFlags & PTF_LEAF)==0 && nOld!=nNew ){
    MemPage *pOld = (nNew>nOld ? apNew : apOld)[nOld-1];
    memcpy(&apNew[nNew-1]->aData[8], &pOld->aData[8], 4);
  }

  /* 在完成平衡操作后，需更新兄弟页上单元格关联的指针映射条目。
  ** 由insertCell()例程负责设置分隔单元格的指针映射条目。相关指针映射条目包括：
  ** a) 若单元格引用了溢出链，则需更新该溢出链首页的指针映射条目
  ** b) 若兄弟页非叶子页，则需更新单元格关联的子页指针映射条目
  **
  ** 注意：若兄弟页非叶子页，每个兄弟页右子指针的指针映射条目可能
  ** 也需要更新。但该操作发生在兄弟页数据填充完成后（下方代码处理），
  ** 而非在此处执行。
  */
  if( ISAUTOVACUUM(pBt) ){
    MemPage *pOld;
    MemPage *pNew = pOld = apNew[0];
    int cntOldNext = pNew->nCell + pNew->nOverflow;
    int iNew = 0;
    int iOld = 0;

    for(i=0; i<b.nCell; i++){
      u8 *pCell = b.apCell[i];
      while( i==cntOldNext ){
        iOld++;
        assert( iOld<nNew || iOld<nOld );
        assert( iOld>=0 && iOld<NB );
        pOld = iOld<nNew ? apNew[iOld] : apOld[iOld];
        cntOldNext += pOld->nCell + pOld->nOverflow + !leafData;
      }
      if( i==cntNew[iNew] ){
        pNew = apNew[++iNew];
        if( !leafData ) continue;
      }

      /* 单元格pCell将被迁移至新兄弟页pNew。该单元格原属：
      ** (1) 兄弟页iOld的组成部分（可能为溢出单元格）
      ** (2) 兄弟页iOld左侧的分隔单元格
      **
      ** 因此，若同时满足以下条件：
      ** a) 兄弟页iOld与pNew具有相同页码
      ** b) pCell确系兄弟页iOrdinary的普通单元格（非分隔/溢出单元格）
      ** 则可跳过指针映射条目的更新操作。*/
      if( iOld>=nNew
       || pNew->pgno!=aPgno[iOld]
       || !SQLITE_WITHIN(pCell,pOld->aData,pOld->aDataEnd)
      ){
        if( !leafCorrection ){
          ptrmapPut(pBt, get4byte(pCell), PTRMAP_BTREE, pNew->pgno, &rc);
        }
        if( cachedCellSize(&b,i)>pNew->minLocal ){
          ptrmapPutOvflPtr(pNew, pOld, pCell, &rc);
        }
        if( rc ) goto balance_cleanup;
      }
    }
  }

  /* Insert new divider cells into pParent. */
  for(i=0; i<nNew-1; i++){
    u8 *pCell;
    u8 *pTemp;
    int sz;
    u8 *pSrcEnd;
    MemPage *pNew = apNew[i];
    j = cntNew[i];

    assert( j<nMaxCells );
    assert( b.apCell[j]!=0 );
    pCell = b.apCell[j];
    sz = b.szCell[j] + leafCorrection;
    pTemp = &aOvflSpace[iOvflSpace];
    if( !pNew->leaf ){
      memcpy(&pNew->aData[8], pCell, 4);
    }else if( leafData ){
      /* 若当前为叶数据树且兄弟页均为叶子页，则：
      ** 1. b.apCell[]数组中不存在显式分隔单元格
      ** 2. 分隔单元格将由上述操作中右兄弟页最右侧单元格的整型键构成
      */
      CellInfo info;
      j--;
      pNew->xParseCell(pNew, b.apCell[j], &info);
      pCell = pTemp;
      sz = 4 + putVarint(&pCell[4], info.nKey);
      pTemp = 0;
    }else{
      pCell -= 4;
      /* 针对非叶数据树的边缘案例：若当前单元格pCell满足：
      ** 1. 曾存储于叶节点
      ** 2. 记录的大小为4字节（btreeParseCellPtr()中定义单元格最小尺寸）
      ** 则其实际尺寸可能小于该值。为确保insertCell()获得精确尺寸，需立即重新解析该单元格。
      ** 此情形仅可能出现在：
      ** a) 用于处理"IN (SELECT...)"子查询的B树
      ** b) 仅含单列主键的WITHOUT ROWID表
      */
      if( b.szCell[j]==4 ){
        assert(leafCorrection==4);
        sz = pParent->xCellSize(pParent, pCell);
      }
    }
    iOvflSpace += sz;
    assert( sz<=pBt->maxLocal+23 );
    assert( iOvflSpace <= (int)pBt->pageSize );
    for(k=0; b.ixNx[k]<=j && ALWAYS(k<NB*2); k++){}
    pSrcEnd = b.apEnd[k];
    if( SQLITE_WITHIN(pSrcEnd, pCell, pCell+sz) ){
      rc = SQLITE_CORRUPT_BKPT;
      goto balance_cleanup;
    }
    rc = insertCell(pParent, nxDiv+i, pCell, sz, pTemp, pNew->pgno);
    if( rc!=SQLITE_OK ) goto balance_cleanup;
    assert( sqlite3PagerIswriteable(pParent->pDbPage) );
  }

  /* 执行兄弟页的实际更新操作时，更新顺序至关重要，需确保不会干扰仍可能被读取的页面。具体规则为：
  **
  ** (1) 左移场景：当单元格从apNew[iPg]移至apNew[iPg-1]
  ** → 必须先更新左侧兄弟页apNew[iPg-1]，再更新当前页apNew[iPg]
  **
  ** (2) 右移场景：当单元格从apNew[iPg]移至apNew[iPg+1]
  ** → 必须先更新右侧兄弟页apNew[iPg+1]，再更新当前页apNew[iPg]
  **
  ** 若上述情况均不适用，则可安全更新该页
  **
  ** 循环中的iPg变量执行两轮遍历：
  ** 1. 首轮下降：从nNew-1递减至0，仅需检测条件(1)
  ** （因前次操作已满足条件(2)）
  ** 2. 次轮上升：从0递增至nNew-1，处理首轮遗漏的页
  ** （此时两条件均已满足）
  */
  for(i=1-nNew; i<nNew; i++){
    int iPg = i<0 ? -i : i;
    assert( iPg>=0 && iPg<nNew );
    if( abDone[iPg] ) continue;         /* Skip pages already processed */
    if( i>=0                            /* On the upwards pass, or... */
     || cntOld[iPg-1]>=cntNew[iPg-1]    /* Condition (1) is true */
    ){
      int iNew;
      int iOld;
      int nNewCell;

      /* Verify condition (1):  If cells are moving left, update iPg
      ** only after iPg-1 has already been updated. */
      assert( iPg==0 || cntOld[iPg-1]>=cntNew[iPg-1] || abDone[iPg-1] );

      /* Verify condition (2):  If cells are moving right, update iPg
      ** only after iPg+1 has already been updated. */
      assert( cntNew[iPg]>=cntOld[iPg] || abDone[iPg+1] );

      if( iPg==0 ){
        iNew = iOld = 0;
        nNewCell = cntNew[0];
      }else{
        iOld = iPg<nOld ? (cntOld[iPg-1] + !leafData) : b.nCell;
        iNew = cntNew[iPg-1] + !leafData;
        nNewCell = cntNew[iPg] - iNew;
      }

      rc = editPage(apNew[iPg], iOld, iNew, nNewCell, &b);
      if( rc ) goto balance_cleanup;
      abDone[iPg]++;
      apNew[iPg]->nFree = usableSpace-szNew[iPg];
      assert( apNew[iPg]->nOverflow==0 );
      assert( apNew[iPg]->nCell==nNewCell );
    }
  }

  /* All pages have been processed exactly once */
  assert( memcmp(abDone, "\01\01\01\01\01", nNew)==0 );

  assert( nOld>0 );
  assert( nNew>0 );

  if( isRoot && pParent->nCell==0 && pParent->hdrOffset<=apNew[0]->nFree ){
    /* 当B树的根页面不再包含任何单元格时，其唯一兄弟页是父页的右子页。此时将子页内容复制至父页，使B树整体高度减一。该操作在部分文档中称为"平衡收缩"子算法。
    **
    ** 对于自动清理型数据库：
    ** 1. copyNodeContent()调用将设置所有与被复制内容相关的指针映射条目
    ** 2. 这些条目对应存储在被复制内容中的数据库映像页指针
    **
    ** 关键操作顺序：
    ** a) 必须先在子页执行碎片整理，再复制到父页
    ** b) 若父页为页1（含数据库头部），其尺寸将小于子页
    ** c) 因此需确保所有空闲空间位于页面首部
    */
    assert( nNew==1 || CORRUPT_DB );
    rc = defragmentPage(apNew[0], -1);
    testcase( rc!=SQLITE_OK );
    assert( apNew[0]->nFree == 
        (get2byteNotZero(&apNew[0]->aData[5]) - apNew[0]->cellOffset
          - apNew[0]->nCell*2)
      || rc!=SQLITE_OK
    );
    copyNodeContent(apNew[0], pParent, &rc);
    freePage(apNew[0], &rc);
  }else if( ISAUTOVACUUM(pBt) && !leafCorrection ){
    /* Fix the pointer map entries associated with the right-child of each
    ** sibling page. All other pointer map entries have already been taken
    ** care of.  */
    for(i=0; i<nNew; i++){
      u32 key = get4byte(&apNew[i]->aData[8]);
      ptrmapPut(pBt, key, PTRMAP_BTREE, apNew[i]->pgno, &rc);
    }
  }

  assert( pParent->isInit );
  TRACE(("BALANCE: finished: old=%d new=%d cells=%d\n",
          nOld, nNew, b.nCell));

  /* Free any old pages that were not reused as new pages.
  */
  for(i=nNew; i<nOld; i++){
    freePage(apOld[i], &rc);
  }

  /*
  ** Cleanup before returning.
  */
balance_cleanup:
  sqlite3StackFree(0, b.apCell);
  for(i=0; i<nOld; i++){
    releasePage(apOld[i]);
  }
  for(i=0; i<nNew; i++){
    releasePage(apNew[i]);
  }

  return rc;
}


/*
** 本函数在B树结构的根页过满（存在一个或多个溢出页）时被调用。
** 
** 主要处理流程：
**   (1) 分配新的子页面
**   (2) 将当前根页的所有内容（包括溢出单元格）复制到新建的子页
**   (3) 将原根页重写为空白页，并使其右子页面指针指向新创建的子页
**
** 返回前完成以下维护操作：
**   a. 更新所有与新子页包含的指针相关的指针映射条目
**   b. 更新根页新右子指针对应的指针映射条目
**
** 执行结果处理：
**   - 成功时：设置*ppChild指向子页引用并返回SQLITE_OK
**     （调用方必须且只能调用一次releasePage()释放*ppChild）
**   - 出错时：返回错误代码并将*ppChild设为0
*/
static int balance_deeper(MemPage *pRoot, MemPage **ppChild){
  int rc;                        /* Return value from subprocedures */
  MemPage *pChild = 0;           /* Pointer to a new child page */
  Pgno pgnoChild = 0;            /* Page number of the new child page */
  BtShared *pBt = pRoot->pBt;    /* The BTree */

  assert( pRoot->nOverflow>0 );
  assert( sqlite3_mutex_held(pBt->mutex) );

  // 申请新页，并将root页内容拷贝到新页上
  rc = sqlite3PagerWrite(pRoot->pDbPage);
  if( rc==SQLITE_OK ){
    rc = allocateBtreePage(pBt,&pChild,&pgnoChild,pRoot->pgno,0);
    copyNodeContent(pRoot, pChild, &rc);
    if( ISAUTOVACUUM(pBt) ){
      ptrmapPut(pBt, pgnoChild, PTRMAP_BTREE, pRoot->pgno, &rc);
    }
  }
  if( rc ){
    *ppChild = 0;
    releasePage(pChild);
    return rc;
  }

  // 更新溢出页信息缓存
  memcpy(pChild->aiOvfl, pRoot->aiOvfl,
         pRoot->nOverflow*sizeof(pRoot->aiOvfl[0]));
  memcpy(pChild->apOvfl, pRoot->apOvfl,
         pRoot->nOverflow*sizeof(pRoot->apOvfl[0]));
  pChild->nOverflow = pRoot->nOverflow;

  // root页清零
  zeroPage(pRoot, pChild->aData[0] & ~PTF_LEAF);
  put4byte(&pRoot->aData[pRoot->hdrOffset+8], pgnoChild);

  *ppChild = pChild;
  return SQLITE_OK;
}

/*
** Return SQLITE_CORRUPT if any cursor other than pCur is currently valid
** on the same B-tree as pCur.
**
** This can occur if a database is corrupt with two or more SQL tables
** pointing to the same b-tree.  If an insert occurs on one SQL table
** and causes a BEFORE TRIGGER to do a secondary insert on the other SQL
** table linked to the same b-tree.  If the secondary insert causes a
** rebalance, that can change content out from under the cursor on the
** first SQL table, violating invariants on the first insert.
*/
static int anotherValidCursor(BtCursor *pCur){
  BtCursor *pOther;
  for(pOther=pCur->pBt->pCursor; pOther; pOther=pOther->pNext){
    if( pOther!=pCur
     && pOther->eState==CURSOR_VALID
     && pOther->pPage==pCur->pPage
    ){
      return SQLITE_CORRUPT_BKPT;
    }
  }
  return SQLITE_OK;
}

/*
** 当前pCur所指向的页面刚刚发生了某种修改。本函数的作用是判断该修改是否导致树结构需要重新平衡，
** 如果需要，则调用相应的平衡处理例程。可用的平衡例程包括：
**
** balance_quick() // 快速平衡
** balance_deeper() // 深度平衡
** balance_nonroot() // 非根节点平衡
*/
static int balance(BtCursor *pCur){
  int rc = SQLITE_OK;
  u8 aBalanceQuickSpace[13];
  u8 *pFree = 0;

  VVA_ONLY( int balance_quick_called = 0 );
  VVA_ONLY( int balance_deeper_called = 0 );

  do {
    int iPage;
    MemPage *pPage = pCur->pPage;

    if( NEVER(pPage->nFree<0) && btreeComputeFreeSpace(pPage) ) break;
    if( pPage->nOverflow==0 && pPage->nFree*3<=(int)pCur->pBt->usableSize*2 ){
      /* 只要满足以下两个条件，就无需进行重新平衡：
      **   (1) 不存在溢出单元格
      **   (2) 页面上的空闲空间小于该页总可用空间的三分之二
      */
      break;
    }else if( (iPage = pCur->iPage)==0 ){
      if( pPage->nOverflow && (rc = anotherValidCursor(pCur))==SQLITE_OK ){
        /* 当B树的根页过满时，调用balance_deeper()函数执行以下操作：
        **   (1) 为根页创建新的子页
        **   (2) 将当前根页的内容复制到新建的子页
        ** 在下一次do循环迭代中，系统将对这个新建的子页进行平衡处理
        */
        assert( balance_deeper_called==0 );
        VVA_ONLY( balance_deeper_called++ );
        rc = balance_deeper(pPage, &pCur->apPage[1]);
        if( rc==SQLITE_OK ){
          pCur->iPage = 1;
          pCur->ix = 0;
          pCur->aiIdx[0] = 0;
          pCur->apPage[0] = pPage;
          pCur->pPage = pCur->apPage[1]; // cursor 移动到新子页上
          assert( pCur->pPage->nOverflow );
        }
      }else{
        break;
      }
    } else {
      MemPage * const pParent = pCur->apPage[iPage-1];
      int const iIdx = pCur->aiIdx[iPage-1];

      rc = sqlite3PagerWrite(pParent->pDbPage);
      if( rc==SQLITE_OK && pParent->nFree<0 ){
        rc = btreeComputeFreeSpace(pParent);
      }
      if( rc==SQLITE_OK ){
#ifndef SQLITE_OMIT_QUICKBALANCE
        if( pPage->intKeyLeaf
         && pPage->nOverflow==1
         && pPage->aiOvfl[0]==pPage->nCell
         && pParent->pgno!=1
         && pParent->nCell==iIdx
        ){
          /* 当需要处理溢出单元格时，调用balance_quick()执行以下操作：
          **   (1) 为pPage创建新的兄弟页面用于存储溢出单元格
          **   (2) 向父页面(pParent)插入新单元格，可能引发父页面溢出
          ** 
          ** 处理流程说明：
          **   a. 若发生父页面溢出，do循环的下一次迭代将通过balance_nonroot()或balance_deeper()
          **      对父页面进行平衡处理
          **   b. 在此期间，溢出单元格临时存储在aBalanceQuickSpace[]缓冲区
          **
          ** 下方assert()的用途：
          **   (安全验证) 确保每次调用本函数时，balance_quick()仅被调用一次
          **   (隐患防御) 若未经验证，可能导致aBalanceQuickSpace[]缓冲区重用相关的隐蔽错误
          */
          assert( balance_quick_called==0 ); 
          VVA_ONLY( balance_quick_called++ );
          rc = balance_quick(pParent, pPage, aBalanceQuickSpace);
        }else
#endif
        {
          /* 当出现这种情况时，调用balance_nonroot()函数执行以下操作：
          **   (1) 在pPage及其最多2个兄弟页面之间重新分配单元格
          **   (2) 修改父页面(pParent)的内容，可能导致父页面出现过载或欠载状态
          ** 
          ** 处理逻辑说明：
          **   a. do循环的下一次迭代将对父页面进行平衡操作以修正上述状态
          **   b. 若父页面过载，溢出的单元格将存储在下文立即分配的pSpace缓冲区中
          **   c. 后续迭代将调用balance_nonroot()处理这些溢出单元（可能先调用balance_deeper()
          **      但该函数仅移动单元格到其他页面而不处理溢出）
          **   d. 当后续balance_nonroot()调用完成后，可安全释放前次使用的pSpace缓冲区，因为：
          **      - 溢出数据已被复制到数据库页主体中
          **      - 或转移到新创建的pSpace缓冲区供后续balance_nonroot()使用
          */
          u8 *pSpace = sqlite3PageMalloc(pCur->pBt->pageSize);
          rc = balance_nonroot(pParent, iIdx, pSpace, iPage==1,
                               pCur->hints&BTREE_BULKLOAD);
          if( pFree ){
            /* If pFree is not NULL, it points to the pSpace buffer used 
            ** by a previous call to balance_nonroot(). Its contents are
            ** now stored either on real database pages or within the 
            ** new pSpace buffer, so it may be safely freed here. */
            sqlite3PageFree(pFree);
          }

          /* The pSpace buffer will be freed after the next call to
          ** balance_nonroot(), or just before this function returns, whichever
          ** comes first. */
          pFree = pSpace;
        }
      }

      pPage->nOverflow = 0;

      /* The next iteration of the do-loop balances the parent page. */
      releasePage(pPage);
      pCur->iPage--;
      assert( pCur->iPage>=0 );
      pCur->pPage = pCur->apPage[pCur->iPage];
    }
  }while( rc==SQLITE_OK );

  if( pFree ){
    sqlite3PageFree(pFree);
  }
  return rc;
}

/* 将pX的内容覆盖写入pDest，仅当目标位置现有内容不同时才执行写入操作 */
static int btreeOverwriteContent(
  MemPage *pPage,           /* MemPage on which writing will occur */
  u8 *pDest,                /* Pointer to the place to start writing */
  const BtreePayload *pX,   /* Source of data to write */
  int iOffset,              /* Offset of first byte to write */
  int iAmt                  /* Number of bytes to be written */
){
  int nData = pX->nData - iOffset;
  if( nData<=0 ){ // 如果nData < iOffset, 说明本次只是将[pDest, pDest + iAmt] 内存memset
    /* Overwritting with zeros */
    int i;
    for(i=0; i<iAmt && pDest[i]==0; i++){}
    if( i<iAmt ){
      int rc = sqlite3PagerWrite(pPage->pDbPage);
      if( rc ) return rc;
      memset(pDest + i, 0, iAmt - i);
    }
  }else{
    if( nData<iAmt ){
      // 处理末尾混合数据与零值：先递归写入零值，然后继续执行写入真实数据 
      int rc = btreeOverwriteContent(pPage, pDest+nData, pX, iOffset+nData,
                                 iAmt-nData);
      if( rc ) return rc;
      iAmt = nData;
    }
    if( memcmp(pDest, ((u8*)pX->pData) + iOffset, iAmt)!=0 ){
      int rc = sqlite3PagerWrite(pPage->pDbPage);
      if( rc ) return rc;
      /* In a corrupt database, it is possible for the source and destination
      ** buffers to overlap.  This is harmless since the database is already
      ** corrupt but it does cause valgrind and ASAN warnings.  So use
      ** memmove(). */
      memmove(pDest, ((u8*)pX->pData) + iOffset, iAmt);
    }
  }
  return SQLITE_OK;
}

// 覆盖写入px->data到pCur指向的Cell
static int btreeOverwriteCell(BtCursor *pCur, const BtreePayload *pX){
  int iOffset;                        /* Next byte of pX->pData to write */
  int nTotal = pX->nData + pX->nZero; /* Total bytes of to write */
  int rc;                             /* Return code */
  MemPage *pPage = pCur->pPage;       /* Page being written */
  BtShared *pBt;                      /* Btree */
  Pgno ovflPgno;                      /* Next overflow page to write */
  u32 ovflPageSize;                   /* Size to write on overflow page */

  if( pCur->info.pPayload + pCur->info.nLocal > pPage->aDataEnd
   || pCur->info.pPayload < pPage->aData + pPage->cellOffset
  ){
    return SQLITE_CORRUPT_BKPT;
  }
  // 先覆写B-Tree页上的数据
  rc = btreeOverwriteContent(pPage, pCur->info.pPayload, pX,
                             0, pCur->info.nLocal);
  if( rc ) return rc;
  if( pCur->info.nLocal==nTotal ) return SQLITE_OK;

  /* Now overwrite the overflow pages */
  iOffset = pCur->info.nLocal;
  assert( nTotal>=0 );
  assert( iOffset>=0 );
  ovflPgno = get4byte(pCur->info.pPayload + iOffset);
  pBt = pPage->pBt;
  ovflPageSize = pBt->usableSize - 4;
  do{
    rc = btreeGetPage(pBt, ovflPgno, &pPage, 0);
    if( rc ) return rc;
    if( sqlite3PagerPageRefcount(pPage->pDbPage)!=1 || pPage->isInit ){
      rc = SQLITE_CORRUPT_BKPT;
    }else{
      if( iOffset+ovflPageSize<(u32)nTotal ){
        ovflPgno = get4byte(pPage->aData);
      }else{
        ovflPageSize = nTotal - iOffset;
      }
      rc = btreeOverwriteContent(pPage, pPage->aData+4, pX,
                                 iOffset, ovflPageSize);
    }
    sqlite3PagerUnref(pPage->pDbPage);
    if( rc ) return rc;
    iOffset += ovflPageSize;
  }while( iOffset<nTotal );
  return SQLITE_OK;    
}


/*
** 向 B 树中插入一条新记录。新记录的内容由 pX 对象描述。
** pCur 游标仅用于确定插入的目标表，并在插入后指向一个随机位置。
**
** 对于表 B 树（用于行 ID 表）：
**   - 仅使用键的 pX.nKey 值，pX.pKey 必须为 NULL。
**   - pX.nKey 表示行的 rowid 或 INTEGER PRIMARY KEY。
**   - pX.nData、pData、nZero 字段存储行的内容。
**
** 对于索引 B 树（用于索引和 WITHOUT ROWID 表）：
**   - 键是存储在 pX.pKey、nKey 中的任意字节序列。
**   - pX.pData、nData、nZero 字段必须为零。
**
** 若 seekResult 参数非零：
**   - 表示已成功调用 sqlite3BtreeIndexMoveto() 将游标 pCur 定位到 (pKey,nKey) 附近。
**   - seekResult < 0：游标指向比 (pKey,nKey) 小的单元格。
**   - seekResult > 0：游标指向比 (pKey,nKey) 大的单元格。
**
** 若 seekResult == 0：
**   - 表示游标指向未知位置，此函数需先定位到 (pKey,nKey) 的正确插入点。
**   - 对于索引 B 树，若 pX->nMem 非零：
**     - pX->aMem 包含未打包的键值指针，可替代 pX->pKey 避免解码键。
*/
int sqlite3BtreeInsert(
  BtCursor *pCur,                /* Insert data into the table of this cursor */
  const BtreePayload *pX,        /* Content of the row to be inserted */
  int flags,                     /* True if this is likely an append */
  int seekResult                 /* Result of prior IndexMoveto() call */
){
  int rc;
  int loc = seekResult;          /* -1: before desired location  +1: after */
  int szNew = 0;
  int idx;
  MemPage *pPage;
  Btree *p = pCur->pBtree;
  unsigned char *oldCell;
  unsigned char *newCell = 0;

  // 保存此表上所有其他打开游标的位置。 tag.xujun
  if( pCur->curFlags & BTCF_Multiple ){
    rc = saveAllCursors(p->pBt, pCur->pgnoRoot, pCur);
    if( rc ) return rc;
  }

  // 表已经发生了变化，被其他cursor修改过，需要重新定位
  if( pCur->eState>=CURSOR_REQUIRESEEK ){
    testcase( pCur->eState==CURSOR_REQUIRESEEK );
    testcase( pCur->eState==CURSOR_FAULT );
    rc = moveToRoot(pCur);
    if( rc && rc!=SQLITE_EMPTY ) return rc;
  }

  // 调用方需保证插入的数据要么是已经编码好的（索引btree）要么是表btree
  assert( (flags & BTREE_PREFORMAT) || (pX->pKey==0)==(pCur->pKeyInfo==0) );

  if( pCur->pKeyInfo==0 ){ // 表btree
    if( p->hasIncrblobCur ){
      invalidateIncrblobCursors(p, pCur->pgnoRoot, pX->nKey, 0);
    }
    if( (pCur->curFlags&BTCF_ValidNKey)!=0 && pX->nKey==pCur->info.nKey ){ // cursor已经找到同一个key
      if( pCur->info.nSize!=0 && pCur->info.nPayload==(u32)pX->nData+pX->nZero){ // 负载长度也相等，原地更新
        return btreeOverwriteCell(pCur, pX);
      }
      assert( loc==0 );
    }else if( loc==0 ){
      // 光标当前不指向要被覆盖的单元格，也不指向相邻单元格。请移动光标，使其指向要被覆盖的单元格或与之相邻的单元格
      rc = sqlite3BtreeTableMoveto(pCur, pX->nKey, 
               (flags & BTREE_APPEND)!=0, &loc);
      if( rc ) return rc;
    }
  }else{ // 索引btree
    assert( (flags & BTREE_SAVEPOSITION)==0 || loc==0 );
    if( loc==0 && (flags & BTREE_SAVEPOSITION)==0 ){
      if( pX->nMem ){
        UnpackedRecord r;
        r.pKeyInfo = pCur->pKeyInfo;
        r.aMem = pX->aMem;
        r.nField = pX->nMem;
        r.default_rc = 0;
        r.eqSeen = 0;
        rc = sqlite3BtreeIndexMoveto(pCur, &r, &loc);
      }else{
        rc = btreeMoveto(pCur, pX->pKey, pX->nKey, 
                    (flags & BTREE_APPEND)!=0, &loc);
      }
      if( rc ) return rc;
    }
    // 此时loc = 0的含义已经发生变化，如果 flags & BTREE_SAVEPOSITION == 0 , 上面的条件中会去搜索到，并设置loc = 0（表示精准匹配）
    // 如果loc = 0 且 flags & BTREE_SAVEPOSITION != 0, 就已经精准匹配
   
    if( loc==0 ){
      getCellInfo(pCur);
      if( pCur->info.nKey==pX->nKey ){  // 负载长度相等
        BtreePayload x2;
        x2.pData = pX->pKey; //pKey是已经序列化好的负载（如果是叶子节点，pKey 包含了key 和 rowId）
        x2.nData = pX->nKey;
        x2.nZero = 0;
        return btreeOverwriteCell(pCur, &x2);
      }
    }
  }
  pPage = pCur->pPage;
  assert( pPage->leaf || !pPage->intKey ); // 页要么是叶子节点，要么是索引btree页
  if( pPage->nFree<0 ){
    rc = btreeComputeFreeSpace(pPage); // nFree 值未初始化，重新计算
    if( rc ) return rc;
  }
  newCell = p->pBt->pTmpSpace;
  if( flags & BTREE_PREFORMAT ){ // 索引btree
    rc = SQLITE_OK;
    szNew = p->pBt->nPreformatSize;
    if( szNew<4 ) szNew = 4;
    if( ISAUTOVACUUM(p->pBt) && szNew>pPage->maxLocal ){
      CellInfo info;
      pPage->xParseCell(pPage, newCell, &info);
      if( info.nPayload!=info.nLocal ){
        Pgno ovfl = get4byte(&newCell[szNew-4]);
        ptrmapPut(p->pBt, ovfl, PTRMAP_OVERFLOW1, pPage->pgno, &rc);
        if( NEVER(rc) ) goto end_insert;
      }
    }
  }else{
    rc = fillInCell(pPage, newCell, pX, &szNew);
    if( rc ) goto end_insert;
  }
  assert( szNew==pPage->xCellSize(pPage, newCell) );
  assert( szNew <= MX_CELL_SIZE(p->pBt) );
  idx = pCur->ix;
  if( loc==0 ){
    CellInfo info;
    rc = sqlite3PagerWrite(pPage->pDbPage);
    if( rc ){
      goto end_insert;
    }
    oldCell = findCell(pPage, idx);
    if( !pPage->leaf ){
      memcpy(newCell, oldCell, 4);
    }
    BTREE_CLEAR_CELL(rc, pPage, oldCell, info);
    invalidateOverflowCache(pCur);
    if( info.nSize==szNew && info.nLocal==info.nPayload && (!ISAUTOVACUUM(p->pBt) || szNew<pPage->minLocal)){ // 如果大小相等，覆写cell。tag.xujun
      memcpy(oldCell, newCell, szNew);
      return SQLITE_OK;
    }
    dropCell(pPage, idx, info.nSize, &rc);
    if( rc ) goto end_insert;
  }else if( loc<0 && pPage->nCell>0 ){
    assert( pPage->leaf );
    idx = ++pCur->ix;
    pCur->curFlags &= ~BTCF_ValidNKey;
  }else{
    assert( pPage->leaf );
  }
  rc = insertCell(pPage, idx, newCell, szNew, 0, 0);
  assert( pPage->nOverflow==0 || rc==SQLITE_OK );
  assert( rc!=SQLITE_OK || pPage->nCell>0 || pPage->nOverflow>0 );

  /*
  ** 若未发生错误且pPage存在溢出单元格，则调用balance()函数
  ** 在树内重新分配单元格。由于balance()可能移动游标，
  ** 需将BtCursor.info.nSize和BTCF_ValidNKey变量清零。
  **
  ** 旧版SQLite通过调用moveToRoot()将游标移回根页面，
  ** 因为balance()过去会使得BtCursor.apPage[]和BtCursor.aiIdx[]失效。
  ** 新版本改为将游标状态设为"invalid"，这使得常见插入操作略微提速。
  **
  ** 这里还有一个微妙但重要的优化：当使用单个游标向整数键B树插入多条记录时
  ** （例如处理"INSERT INTO ... SELECT"语句期间），
  ** 尽可能保持游标指向B树的最后一条条目。若游标停留在表的最后一条条目，
  ** 且下一条插入记录的整型键大于现有最大键值，
  ** 则无需重新定位游标即可完成插入，这将带来显著的性能提升。
  */
  pCur->info.nSize = 0;
  if( pPage->nOverflow ){
    assert( rc==SQLITE_OK );
    pCur->curFlags &= ~(BTCF_ValidNKey);
    rc = balance(pCur);

    /* Must make sure nOverflow is reset to zero even if the balance()
    ** fails. Internal data structure corruption will result otherwise. 
    ** Also, set the cursor state to invalid. This stops saveCursorPosition()
    ** from trying to save the current position of the cursor.  */
    pCur->pPage->nOverflow = 0;
    pCur->eState = CURSOR_INVALID;
    if( (flags & BTREE_SAVEPOSITION) && rc==SQLITE_OK ){
      btreeReleaseAllCursorPages(pCur);
      if( pCur->pKeyInfo ){
        assert( pCur->pKey==0 );
        pCur->pKey = sqlite3Malloc( pX->nKey );
        if( pCur->pKey==0 ){
          rc = SQLITE_NOMEM;
        }else{
          memcpy(pCur->pKey, pX->pKey, pX->nKey);
        }
      }
      pCur->eState = CURSOR_REQUIRESEEK;
      pCur->nKey = pX->nKey;
    }
  }
  assert( pCur->iPage<0 || pCur->pPage->nOverflow==0 );

end_insert:
  return rc;
}

/*
** This function is used as part of copying the current row from cursor
** pSrc into cursor pDest. If the cursors are open on intkey tables, then
** parameter iKey is used as the rowid value when the record is copied
** into pDest. Otherwise, the record is copied verbatim.
**
** This function does not actually write the new value to cursor pDest.
** Instead, it creates and populates any required overflow pages and
** writes the data for the new cell into the BtShared.pTmpSpace buffer
** for the destination database. The size of the cell, in bytes, is left
** in BtShared.nPreformatSize. The caller completes the insertion by
** calling sqlite3BtreeInsert() with the BTREE_PREFORMAT flag specified.
**
** SQLITE_OK is returned if successful, or an SQLite error code otherwise.
*/
int sqlite3BtreeTransferRow(BtCursor *pDest, BtCursor *pSrc, i64 iKey){
  BtShared *pBt = pDest->pBt;
  u8 *aOut = pBt->pTmpSpace;    /* Pointer to next output buffer */
  const u8 *aIn;                /* Pointer to next input buffer */
  u32 nIn;                      /* Size of input buffer aIn[] */
  u32 nRem;                     /* Bytes of data still to copy */

  getCellInfo(pSrc);
  if( pSrc->info.nPayload<0x80 ){
    *(aOut++) = pSrc->info.nPayload;
  }else{
    aOut += sqlite3PutVarint(aOut, pSrc->info.nPayload);
  }
  if( pDest->pKeyInfo==0 ) aOut += putVarint(aOut, iKey);
  nIn = pSrc->info.nLocal;
  aIn = pSrc->info.pPayload;
  if( aIn+nIn>pSrc->pPage->aDataEnd ){
    return SQLITE_CORRUPT_BKPT;
  }
  nRem = pSrc->info.nPayload;
  if( nIn==nRem && nIn<pDest->pPage->maxLocal ){
    memcpy(aOut, aIn, nIn);
    pBt->nPreformatSize = nIn + (aOut - pBt->pTmpSpace);
    return SQLITE_OK;
  }else{
    int rc = SQLITE_OK;
    Pager *pSrcPager = pSrc->pBt->pPager;
    u8 *pPgnoOut = 0;
    Pgno ovflIn = 0;
    DbPage *pPageIn = 0;
    MemPage *pPageOut = 0;
    u32 nOut;                     /* Size of output buffer aOut[] */

    nOut = btreePayloadToLocal(pDest->pPage, pSrc->info.nPayload);
    pBt->nPreformatSize = nOut + (aOut - pBt->pTmpSpace);
    if( nOut<pSrc->info.nPayload ){
      pPgnoOut = &aOut[nOut];
      pBt->nPreformatSize += 4;
    }
  
    if( nRem>nIn ){
      if( aIn+nIn+4>pSrc->pPage->aDataEnd ){
        return SQLITE_CORRUPT_BKPT;
      }
      ovflIn = get4byte(&pSrc->info.pPayload[nIn]);
    }
  
    do {
      nRem -= nOut;
      do{
        assert( nOut>0 );
        if( nIn>0 ){
          int nCopy = MIN(nOut, nIn);
          memcpy(aOut, aIn, nCopy);
          nOut -= nCopy;
          nIn -= nCopy;
          aOut += nCopy;
          aIn += nCopy;
        }
        if( nOut>0 ){
          sqlite3PagerUnref(pPageIn);
          pPageIn = 0;
          rc = sqlite3PagerGet(pSrcPager, ovflIn, &pPageIn, PAGER_GET_READONLY);
          if( rc==SQLITE_OK ){
            aIn = (const u8*)sqlite3PagerGetData(pPageIn);
            ovflIn = get4byte(aIn);
            aIn += 4;
            nIn = pSrc->pBt->usableSize - 4;
          }
        }
      }while( rc==SQLITE_OK && nOut>0 );
  
      if( rc==SQLITE_OK && nRem>0 && ALWAYS(pPgnoOut) ){
        Pgno pgnoNew;
        MemPage *pNew = 0;
        rc = allocateBtreePage(pBt, &pNew, &pgnoNew, 0, 0);
        put4byte(pPgnoOut, pgnoNew);
        if( ISAUTOVACUUM(pBt) && pPageOut ){
          ptrmapPut(pBt, pgnoNew, PTRMAP_OVERFLOW2, pPageOut->pgno, &rc);
        }
        releasePage(pPageOut);
        pPageOut = pNew;
        if( pPageOut ){
          pPgnoOut = pPageOut->aData;
          put4byte(pPgnoOut, 0);
          aOut = &pPgnoOut[4];
          nOut = MIN(pBt->usableSize - 4, nRem);
        }
      }
    }while( nRem>0 && rc==SQLITE_OK );
  
    releasePage(pPageOut);
    sqlite3PagerUnref(pPageIn);
    return rc;
  }
}

/*
** Delete the entry that the cursor is pointing to. 
**
** If the BTREE_SAVEPOSITION bit of the flags parameter is zero, then
** the cursor is left pointing at an arbitrary location after the delete.
** But if that bit is set, then the cursor is left in a state such that
** the next call to BtreeNext() or BtreePrev() moves it to the same row
** as it would have been on if the call to BtreeDelete() had been omitted.
**
** The BTREE_AUXDELETE bit of flags indicates that is one of several deletes
** associated with a single table entry and its indexes.  Only one of those
** deletes is considered the "primary" delete.  The primary delete occurs
** on a cursor that is not a BTREE_FORDELETE cursor.  All but one delete
** operation on non-FORDELETE cursors is tagged with the AUXDELETE flag.
** The BTREE_AUXDELETE bit is a hint that is not used by this implementation,
** but which might be used by alternative storage engines.
*/
int sqlite3BtreeDelete(BtCursor *pCur, u8 flags){
  Btree *p = pCur->pBtree;
  BtShared *pBt = p->pBt;              
  int rc;                    /* Return code */
  MemPage *pPage;            /* Page to delete cell from */
  unsigned char *pCell;      /* Pointer to cell to delete */
  int iCellIdx;              /* Index of cell to delete */
  int iCellDepth;            /* Depth of node containing pCell */ 
  CellInfo info;             /* Size of the cell being deleted */
  u8 bPreserve;              /* Keep cursor valid.  2 for CURSOR_SKIPNEXT */

  assert( cursorOwnsBtShared(pCur) );
  assert( pBt->inTransaction==TRANS_WRITE );
  assert( (pBt->btsFlags & BTS_READ_ONLY)==0 );
  assert( pCur->curFlags & BTCF_WriteFlag );
  assert( hasSharedCacheTableLock(p, pCur->pgnoRoot, pCur->pKeyInfo!=0, 2) );
  assert( !hasReadConflicts(p, pCur->pgnoRoot) );
  assert( (flags & ~(BTREE_SAVEPOSITION | BTREE_AUXDELETE))==0 );
  if( pCur->eState!=CURSOR_VALID ){
    if( pCur->eState>=CURSOR_REQUIRESEEK ){
      rc = btreeRestoreCursorPosition(pCur);
      assert( rc!=SQLITE_OK || CORRUPT_DB || pCur->eState==CURSOR_VALID );
      if( rc || pCur->eState!=CURSOR_VALID ) return rc;
    }else{
      return SQLITE_CORRUPT_BKPT;
    }
  }
  assert( pCur->eState==CURSOR_VALID );

  iCellDepth = pCur->iPage;
  iCellIdx = pCur->ix;
  pPage = pCur->pPage;
  if( pPage->nCell<=iCellIdx ){
    return SQLITE_CORRUPT_BKPT;
  }
  pCell = findCell(pPage, iCellIdx);
  if( pPage->nFree<0 && btreeComputeFreeSpace(pPage) ){
    return SQLITE_CORRUPT_BKPT;
  }

  /* If the BTREE_SAVEPOSITION bit is on, then the cursor position must
  ** be preserved following this delete operation. If the current delete
  ** will cause a b-tree rebalance, then this is done by saving the cursor
  ** key and leaving the cursor in CURSOR_REQUIRESEEK state before 
  ** returning. 
  **
  ** If the current delete will not cause a rebalance, then the cursor
  ** will be left in CURSOR_SKIPNEXT state pointing to the entry immediately
  ** before or after the deleted entry.
  **
  ** The bPreserve value records which path is required:
  **
  **    bPreserve==0         Not necessary to save the cursor position
  **    bPreserve==1         Use CURSOR_REQUIRESEEK to save the cursor position
  **    bPreserve==2         Cursor won't move.  Set CURSOR_SKIPNEXT.
  */
  bPreserve = (flags & BTREE_SAVEPOSITION)!=0;
  if( bPreserve ){
    if( !pPage->leaf 
     || (pPage->nFree+pPage->xCellSize(pPage,pCell)+2) >
                                                   (int)(pBt->usableSize*2/3)
     || pPage->nCell==1  /* See dbfuzz001.test for a test case */
    ){
      /* A b-tree rebalance will be required after deleting this entry.
      ** Save the cursor key.  */
      rc = saveCursorKey(pCur);
      if( rc ) return rc;
    }else{
      bPreserve = 2;
    }
  }

  /* If the page containing the entry to delete is not a leaf page, move
  ** the cursor to the largest entry in the tree that is smaller than
  ** the entry being deleted. This cell will replace the cell being deleted
  ** from the internal node. The 'previous' entry is used for this instead
  ** of the 'next' entry, as the previous entry is always a part of the
  ** sub-tree headed by the child page of the cell being deleted. This makes
  ** balancing the tree following the delete operation easier.  */
  if( !pPage->leaf ){
    rc = sqlite3BtreePrevious(pCur, 0);
    assert( rc!=SQLITE_DONE );
    if( rc ) return rc;
  }

  /* Save the positions of any other cursors open on this table before
  ** making any modifications.  */
  if( pCur->curFlags & BTCF_Multiple ){
    rc = saveAllCursors(pBt, pCur->pgnoRoot, pCur);
    if( rc ) return rc;
  }

  /* If this is a delete operation to remove a row from a table b-tree,
  ** invalidate any incrblob cursors open on the row being deleted.  */
  if( pCur->pKeyInfo==0 && p->hasIncrblobCur ){
    invalidateIncrblobCursors(p, pCur->pgnoRoot, pCur->info.nKey, 0);
  }

  /* Make the page containing the entry to be deleted writable. Then free any
  ** overflow pages associated with the entry and finally remove the cell
  ** itself from within the page.  */
  rc = sqlite3PagerWrite(pPage->pDbPage);
  if( rc ) return rc;
  BTREE_CLEAR_CELL(rc, pPage, pCell, info);
  dropCell(pPage, iCellIdx, info.nSize, &rc);
  if( rc ) return rc;

  /* If the cell deleted was not located on a leaf page, then the cursor
  ** is currently pointing to the largest entry in the sub-tree headed
  ** by the child-page of the cell that was just deleted from an internal
  ** node. The cell from the leaf node needs to be moved to the internal
  ** node to replace the deleted cell.  */
  if( !pPage->leaf ){
    MemPage *pLeaf = pCur->pPage;
    int nCell;
    Pgno n;
    unsigned char *pTmp;

    if( pLeaf->nFree<0 ){
      rc = btreeComputeFreeSpace(pLeaf);
      if( rc ) return rc;
    }
    if( iCellDepth<pCur->iPage-1 ){
      n = pCur->apPage[iCellDepth+1]->pgno;
    }else{
      n = pCur->pPage->pgno;
    }
    pCell = findCell(pLeaf, pLeaf->nCell-1);
    if( pCell<&pLeaf->aData[4] ) return SQLITE_CORRUPT_BKPT;
    nCell = pLeaf->xCellSize(pLeaf, pCell);
    assert( MX_CELL_SIZE(pBt) >= nCell );
    pTmp = pBt->pTmpSpace;
    assert( pTmp!=0 );
    rc = sqlite3PagerWrite(pLeaf->pDbPage);
    if( rc==SQLITE_OK ){
      rc = insertCell(pPage, iCellIdx, pCell-4, nCell+4, pTmp, n);
    }
    dropCell(pLeaf, pLeaf->nCell-1, nCell, &rc);
    if( rc ) return rc;
  }

  /* Balance the tree. If the entry deleted was located on a leaf page,
  ** then the cursor still points to that page. In this case the first
  ** call to balance() repairs the tree, and the if(...) condition is
  ** never true.
  **
  ** Otherwise, if the entry deleted was on an internal node page, then
  ** pCur is pointing to the leaf page from which a cell was removed to
  ** replace the cell deleted from the internal node. This is slightly
  ** tricky as the leaf node may be underfull, and the internal node may
  ** be either under or overfull. In this case run the balancing algorithm
  ** on the leaf node first. If the balance proceeds far enough up the
  ** tree that we can be sure that any problem in the internal node has
  ** been corrected, so be it. Otherwise, after balancing the leaf node,
  ** walk the cursor up the tree to the internal node and balance it as 
  ** well.  */
  assert( pCur->pPage->nOverflow==0 );
  assert( pCur->pPage->nFree>=0 );
  if( pCur->pPage->nFree*3<=(int)pCur->pBt->usableSize*2 ){
    /* Optimization: If the free space is less than 2/3rds of the page,
    ** then balance() will always be a no-op.  No need to invoke it. */
    rc = SQLITE_OK;
  }else{
    rc = balance(pCur);
  }
  if( rc==SQLITE_OK && pCur->iPage>iCellDepth ){
    releasePageNotNull(pCur->pPage);
    pCur->iPage--;
    while( pCur->iPage>iCellDepth ){
      releasePage(pCur->apPage[pCur->iPage--]);
    }
    pCur->pPage = pCur->apPage[pCur->iPage];
    rc = balance(pCur);
  }

  if( rc==SQLITE_OK ){
    if( bPreserve>1 ){
      assert( (pCur->iPage==iCellDepth || CORRUPT_DB) );
      assert( pPage==pCur->pPage || CORRUPT_DB );
      assert( (pPage->nCell>0 || CORRUPT_DB) && iCellIdx<=pPage->nCell );
      pCur->eState = CURSOR_SKIPNEXT;
      if( iCellIdx>=pPage->nCell ){
        pCur->skipNext = -1;
        pCur->ix = pPage->nCell-1;
      }else{
        pCur->skipNext = 1;
      }
    }else{
      rc = moveToRoot(pCur);
      if( bPreserve ){
        btreeReleaseAllCursorPages(pCur);
        pCur->eState = CURSOR_REQUIRESEEK;
      }
      if( rc==SQLITE_EMPTY ) rc = SQLITE_OK;
    }
  }
  return rc;
}

/*
** 创建新的BTree表，并将新表根页面的页码存入*piTable
**
** 表的类型由flags参数决定。当前仅支持以下flags取值，
** 其他取值可能导致不可预期行为：
**
** BTREE_INTKEY|BTREE_LEAFDATA —— 用于含rowid键的SQL数据表
** BTREE_ZERODATA —— 用于SQL索引
*/
static int btreeCreateTable(Btree *p, Pgno *piTable, int createTabFlags){
  BtShared *pBt = p->pBt;
  MemPage *pRoot;
  Pgno pgnoRoot;
  int rc;
  int ptfFlags;          /* Page-type flage for the root page of new table */

  assert( sqlite3BtreeHoldsMutex(p) );
  assert( pBt->inTransaction==TRANS_WRITE );
  assert( (pBt->btsFlags & BTS_READ_ONLY)==0 );

#ifdef SQLITE_OMIT_AUTOVACUUM
  rc = allocateBtreePage(pBt, &pRoot, &pgnoRoot, 1, 0);
  if( rc ){
    return rc;
  }
#else
  if( pBt->autoVacuum ){
    Pgno pgnoMove;      /* Move a page here to make room for the root-page */
    MemPage *pPageMove; /* The page to move to. */

    /*
    ** 创建新表时可能需要移动现有数据库，为新表的根页面腾出存储空间。
    ** 若检测到该根页面实际为溢出页，将删除由已打开游标持有的所有溢出页映射缓存。
    */
    invalidateAllOverflowCache(pBt);

    /* Read the value of meta[3] from the database to determine where the
    ** root page of the new table should go. meta[3] is the largest root-page
    ** created so far, so the new root-page is (meta[3]+1).
    */
    sqlite3BtreeGetMeta(p, BTREE_LARGEST_ROOT_PAGE, &pgnoRoot);
    if( pgnoRoot>btreePagecount(pBt) ){
      return SQLITE_CORRUPT_BKPT;
    }
    pgnoRoot++;

    /* The new root-page may not be allocated on a pointer-map page, or the
    ** PENDING_BYTE page.
    */
    while( pgnoRoot==PTRMAP_PAGENO(pBt, pgnoRoot) ||
        pgnoRoot==PENDING_BYTE_PAGE(pBt) ){
      pgnoRoot++;
    }
    assert( pgnoRoot>=3 );

    /* Allocate a page. The page that currently resides at pgnoRoot will
    ** be moved to the allocated page (unless the allocated page happens
    ** to reside at pgnoRoot).
    */
    rc = allocateBtreePage(pBt, &pPageMove, &pgnoMove, pgnoRoot, BTALLOC_EXACT);
    if( rc!=SQLITE_OK ){
      return rc;
    }

    if( pgnoMove!=pgnoRoot ){
      /* pgnoRoot is the page that will be used for the root-page of
      ** the new table (assuming an error did not occur). But we were
      ** allocated pgnoMove. If required (i.e. if it was not allocated
      ** by extending the file), the current page at position pgnoMove
      ** is already journaled.
      */
      u8 eType = 0;
      Pgno iPtrPage = 0;

      /* Save the positions of any open cursors. This is required in
      ** case they are holding a reference to an xFetch reference
      ** corresponding to page pgnoRoot.  */
      rc = saveAllCursors(pBt, 0, 0);
      releasePage(pPageMove);
      if( rc!=SQLITE_OK ){
        return rc;
      }

      /* Move the page currently at pgnoRoot to pgnoMove. */
      rc = btreeGetPage(pBt, pgnoRoot, &pRoot, 0);
      if( rc!=SQLITE_OK ){
        return rc;
      }
      rc = ptrmapGet(pBt, pgnoRoot, &eType, &iPtrPage);
      if( eType==PTRMAP_ROOTPAGE || eType==PTRMAP_FREEPAGE ){
        rc = SQLITE_CORRUPT_BKPT;
      }
      if( rc!=SQLITE_OK ){
        releasePage(pRoot);
        return rc;
      }
      assert( eType!=PTRMAP_ROOTPAGE );
      assert( eType!=PTRMAP_FREEPAGE );
      rc = relocatePage(pBt, pRoot, eType, iPtrPage, pgnoMove, 0);
      releasePage(pRoot);

      /* Obtain the page at pgnoRoot */
      if( rc!=SQLITE_OK ){
        return rc;
      }
      rc = btreeGetPage(pBt, pgnoRoot, &pRoot, 0);
      if( rc!=SQLITE_OK ){
        return rc;
      }
      rc = sqlite3PagerWrite(pRoot->pDbPage);
      if( rc!=SQLITE_OK ){
        releasePage(pRoot);
        return rc;
      }
    }else{
      pRoot = pPageMove;
    } 

    /* Update the pointer-map and meta-data with the new root-page number. */
    ptrmapPut(pBt, pgnoRoot, PTRMAP_ROOTPAGE, 0, &rc);
    if( rc ){
      releasePage(pRoot);
      return rc;
    }

    /* When the new root page was allocated, page 1 was made writable in
    ** order either to increase the database filesize, or to decrement the
    ** freelist count.  Hence, the sqlite3BtreeUpdateMeta() call cannot fail.
    */
    assert( sqlite3PagerIswriteable(pBt->pPage1->pDbPage) );
    rc = sqlite3BtreeUpdateMeta(p, 4, pgnoRoot);
    if( NEVER(rc) ){
      releasePage(pRoot);
      return rc;
    }

  }else{
    rc = allocateBtreePage(pBt, &pRoot, &pgnoRoot, 1, 0);
    if( rc ) return rc;
  }
#endif
  assert( sqlite3PagerIswriteable(pRoot->pDbPage) );
  if( createTabFlags & BTREE_INTKEY ){
    ptfFlags = PTF_INTKEY | PTF_LEAFDATA | PTF_LEAF;
  }else{
    ptfFlags = PTF_ZERODATA | PTF_LEAF;
  }
  zeroPage(pRoot, ptfFlags);
  sqlite3PagerUnref(pRoot->pDbPage);
  assert( (pBt->openFlags & BTREE_SINGLE)==0 || pgnoRoot==2 );
  *piTable = pgnoRoot;
  return SQLITE_OK;
}
int sqlite3BtreeCreateTable(Btree *p, Pgno *piTable, int flags){
  int rc;
  sqlite3BtreeEnter(p);
  rc = btreeCreateTable(p, piTable, flags);
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** Erase the given database page and all its children.  Return
** the page to the freelist.
*/
static int clearDatabasePage(
  BtShared *pBt,           /* The BTree that contains the table */
  Pgno pgno,               /* Page number to clear */
  int freePageFlag,        /* Deallocate page if true */
  i64 *pnChange            /* Add number of Cells freed to this counter */
){
  MemPage *pPage;
  int rc;
  unsigned char *pCell;
  int i;
  int hdr;
  CellInfo info;

  assert( sqlite3_mutex_held(pBt->mutex) );
  if( pgno>btreePagecount(pBt) ){
    return SQLITE_CORRUPT_BKPT;
  }
  rc = getAndInitPage(pBt, pgno, &pPage, 0, 0);
  if( rc ) return rc;
  if( (pBt->openFlags & BTREE_SINGLE)==0 
   && sqlite3PagerPageRefcount(pPage->pDbPage) != (1 + (pgno==1))
  ){
    rc = SQLITE_CORRUPT_BKPT;
    goto cleardatabasepage_out;
  }
  hdr = pPage->hdrOffset;
  for(i=0; i<pPage->nCell; i++){
    pCell = findCell(pPage, i);
    if( !pPage->leaf ){
      rc = clearDatabasePage(pBt, get4byte(pCell), 1, pnChange);
      if( rc ) goto cleardatabasepage_out;
    }
    BTREE_CLEAR_CELL(rc, pPage, pCell, info);
    if( rc ) goto cleardatabasepage_out;
  }
  if( !pPage->leaf ){
    rc = clearDatabasePage(pBt, get4byte(&pPage->aData[hdr+8]), 1, pnChange);
    if( rc ) goto cleardatabasepage_out;
    if( pPage->intKey ) pnChange = 0;
  }
  if( pnChange ){
    testcase( !pPage->intKey );
    *pnChange += pPage->nCell;
  }
  if( freePageFlag ){
    freePage(pPage, &rc);
  }else if( (rc = sqlite3PagerWrite(pPage->pDbPage))==0 ){
    zeroPage(pPage, pPage->aData[hdr] | PTF_LEAF);
  }

cleardatabasepage_out:
  releasePage(pPage);
  return rc;
}

/*
** Delete all information from a single table in the database.  iTable is
** the page number of the root of the table.  After this routine returns,
** the root page is empty, but still exists.
**
** This routine will fail with SQLITE_LOCKED if there are any open
** read cursors on the table.  Open write cursors are moved to the
** root of the table.
**
** If pnChange is not NULL, then the integer value pointed to by pnChange
** is incremented by the number of entries in the table.
*/
int sqlite3BtreeClearTable(Btree *p, int iTable, i64 *pnChange){
  int rc;
  BtShared *pBt = p->pBt;
  sqlite3BtreeEnter(p);
  assert( p->inTrans==TRANS_WRITE );

  rc = saveAllCursors(pBt, (Pgno)iTable, 0);

  if( SQLITE_OK==rc ){
    /* Invalidate all incrblob cursors open on table iTable (assuming iTable
    ** is the root of a table b-tree - if it is not, the following call is
    ** a no-op).  */
    if( p->hasIncrblobCur ){
      invalidateIncrblobCursors(p, (Pgno)iTable, 0, 1);
    }
    rc = clearDatabasePage(pBt, (Pgno)iTable, 0, pnChange);
  }
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** Delete all information from the single table that pCur is open on.
**
** This routine only work for pCur on an ephemeral table.
*/
int sqlite3BtreeClearTableOfCursor(BtCursor *pCur){
  return sqlite3BtreeClearTable(pCur->pBtree, pCur->pgnoRoot, 0);
}

/*
** Erase all information in a table and add the root of the table to
** the freelist.  Except, the root of the principle table (the one on
** page 1) is never added to the freelist.
**
** This routine will fail with SQLITE_LOCKED if there are any open
** cursors on the table.
**
** If AUTOVACUUM is enabled and the page at iTable is not the last
** root page in the database file, then the last root page 
** in the database file is moved into the slot formerly occupied by
** iTable and that last slot formerly occupied by the last root page
** is added to the freelist instead of iTable.  In this say, all
** root pages are kept at the beginning of the database file, which
** is necessary for AUTOVACUUM to work right.  *piMoved is set to the 
** page number that used to be the last root page in the file before
** the move.  If no page gets moved, *piMoved is set to 0.
** The last root page is recorded in meta[3] and the value of
** meta[3] is updated by this procedure.
*/
static int btreeDropTable(Btree *p, Pgno iTable, int *piMoved){
  int rc;
  MemPage *pPage = 0;
  BtShared *pBt = p->pBt;

  assert( sqlite3BtreeHoldsMutex(p) );
  assert( p->inTrans==TRANS_WRITE );
  assert( iTable>=2 );
  if( iTable>btreePagecount(pBt) ){
    return SQLITE_CORRUPT_BKPT;
  }

  rc = sqlite3BtreeClearTable(p, iTable, 0);
  if( rc ) return rc;
  rc = btreeGetPage(pBt, (Pgno)iTable, &pPage, 0);
  if( NEVER(rc) ){
    releasePage(pPage);
    return rc;
  }

  *piMoved = 0;

#ifdef SQLITE_OMIT_AUTOVACUUM
  freePage(pPage, &rc);
  releasePage(pPage);
#else
  if( pBt->autoVacuum ){
    Pgno maxRootPgno;
    sqlite3BtreeGetMeta(p, BTREE_LARGEST_ROOT_PAGE, &maxRootPgno);

    if( iTable==maxRootPgno ){
      /* If the table being dropped is the table with the largest root-page
      ** number in the database, put the root page on the free list. 
      */
      freePage(pPage, &rc);
      releasePage(pPage);
      if( rc!=SQLITE_OK ){
        return rc;
      }
    }else{
      /* The table being dropped does not have the largest root-page
      ** number in the database. So move the page that does into the 
      ** gap left by the deleted root-page.
      */
      MemPage *pMove;
      releasePage(pPage);
      rc = btreeGetPage(pBt, maxRootPgno, &pMove, 0);
      if( rc!=SQLITE_OK ){
        return rc;
      }
      rc = relocatePage(pBt, pMove, PTRMAP_ROOTPAGE, 0, iTable, 0);
      releasePage(pMove);
      if( rc!=SQLITE_OK ){
        return rc;
      }
      pMove = 0;
      rc = btreeGetPage(pBt, maxRootPgno, &pMove, 0);
      freePage(pMove, &rc);
      releasePage(pMove);
      if( rc!=SQLITE_OK ){
        return rc;
      }
      *piMoved = maxRootPgno;
    }

    /* Set the new 'max-root-page' value in the database header. This
    ** is the old value less one, less one more if that happens to
    ** be a root-page number, less one again if that is the
    ** PENDING_BYTE_PAGE.
    */
    maxRootPgno--;
    while( maxRootPgno==PENDING_BYTE_PAGE(pBt)
           || PTRMAP_ISPAGE(pBt, maxRootPgno) ){
      maxRootPgno--;
    }
    assert( maxRootPgno!=PENDING_BYTE_PAGE(pBt) );

    rc = sqlite3BtreeUpdateMeta(p, 4, maxRootPgno);
  }else{
    freePage(pPage, &rc);
    releasePage(pPage);
  }
#endif
  return rc;  
}
int sqlite3BtreeDropTable(Btree *p, int iTable, int *piMoved){
  int rc;
  sqlite3BtreeEnter(p);
  rc = btreeDropTable(p, iTable, piMoved);
  sqlite3BtreeLeave(p);
  return rc;
}


/*
** This function may only be called if the b-tree connection already
** has a read or write transaction open on the database.
**
** Read the meta-information out of a database file.  Meta[0]
** is the number of free pages currently in the database.  Meta[1]
** through meta[15] are available for use by higher layers.  Meta[0]
** is read-only, the others are read/write.
** 
** The schema layer numbers meta values differently.  At the schema
** layer (and the SetCookie and ReadCookie opcodes) the number of
** free pages is not visible.  So Cookie[0] is the same as Meta[1].
**
** This routine treats Meta[BTREE_DATA_VERSION] as a special case.  Instead
** of reading the value out of the header, it instead loads the "DataVersion"
** from the pager.  The BTREE_DATA_VERSION value is not actually stored in the
** database file.  It is a number computed by the pager.  But its access
** pattern is the same as header meta values, and so it is convenient to
** read it from this routine.
*/
void sqlite3BtreeGetMeta(Btree *p, int idx, u32 *pMeta){
  BtShared *pBt = p->pBt;

  sqlite3BtreeEnter(p);
  assert( p->inTrans>TRANS_NONE );
  assert( SQLITE_OK==querySharedCacheTableLock(p, SCHEMA_ROOT, READ_LOCK) );
  assert( pBt->pPage1 );
  assert( idx>=0 && idx<=15 );

  if( idx==BTREE_DATA_VERSION ){
    *pMeta = sqlite3PagerDataVersion(pBt->pPager) + p->iBDataVersion;
  }else{
    *pMeta = get4byte(&pBt->pPage1->aData[36 + idx*4]);
  }

  /* If auto-vacuum is disabled in this build and this is an auto-vacuum
  ** database, mark the database as read-only.  */
#ifdef SQLITE_OMIT_AUTOVACUUM
  if( idx==BTREE_LARGEST_ROOT_PAGE && *pMeta>0 ){
    pBt->btsFlags |= BTS_READ_ONLY;
  }
#endif

  sqlite3BtreeLeave(p);
}

/*
** Write meta-information back into the database.  Meta[0] is
** read-only and may not be written.
*/
int sqlite3BtreeUpdateMeta(Btree *p, int idx, u32 iMeta){
  BtShared *pBt = p->pBt;
  unsigned char *pP1;
  int rc;
  assert( idx>=1 && idx<=15 );
  sqlite3BtreeEnter(p);
  assert( p->inTrans==TRANS_WRITE );
  assert( pBt->pPage1!=0 );
  pP1 = pBt->pPage1->aData;
  rc = sqlite3PagerWrite(pBt->pPage1->pDbPage);
  if( rc==SQLITE_OK ){
    put4byte(&pP1[36 + idx*4], iMeta);
#ifndef SQLITE_OMIT_AUTOVACUUM
    if( idx==BTREE_INCR_VACUUM ){
      assert( pBt->autoVacuum || iMeta==0 );
      assert( iMeta==0 || iMeta==1 );
      pBt->incrVacuum = (u8)iMeta;
    }
#endif
  }
  sqlite3BtreeLeave(p);
  return rc;
}

/*
** The first argument, pCur, is a cursor opened on some b-tree. Count the
** number of entries in the b-tree and write the result to *pnEntry.
**
** SQLITE_OK is returned if the operation is successfully executed. 
** Otherwise, if an error is encountered (i.e. an IO error or database
** corruption) an SQLite error code is returned.
*/
int sqlite3BtreeCount(sqlite3 *db, BtCursor *pCur, i64 *pnEntry){
  i64 nEntry = 0;                      /* Value to return in *pnEntry */
  int rc;                              /* Return code */

  rc = moveToRoot(pCur);
  if( rc==SQLITE_EMPTY ){
    *pnEntry = 0;
    return SQLITE_OK;
  }

  /* Unless an error occurs, the following loop runs one iteration for each
  ** page in the B-Tree structure (not including overflow pages). 
  */
  while( rc==SQLITE_OK && !AtomicLoad(&db->u1.isInterrupted) ){
    int iIdx;                          /* Index of child node in parent */
    MemPage *pPage;                    /* Current page of the b-tree */

    /* If this is a leaf page or the tree is not an int-key tree, then 
    ** this page contains countable entries. Increment the entry counter
    ** accordingly.
    */
    pPage = pCur->pPage;
    if( pPage->leaf || !pPage->intKey ){
      nEntry += pPage->nCell;
    }

    /* pPage is a leaf node. This loop navigates the cursor so that it 
    ** points to the first interior cell that it points to the parent of
    ** the next page in the tree that has not yet been visited. The
    ** pCur->aiIdx[pCur->iPage] value is set to the index of the parent cell
    ** of the page, or to the number of cells in the page if the next page
    ** to visit is the right-child of its parent.
    **
    ** If all pages in the tree have been visited, return SQLITE_OK to the
    ** caller.
    */
    if( pPage->leaf ){
      do {
        if( pCur->iPage==0 ){
          /* All pages of the b-tree have been visited. Return successfully. */
          *pnEntry = nEntry;
          return moveToRoot(pCur);
        }
        moveToParent(pCur);
      }while ( pCur->ix>=pCur->pPage->nCell );

      pCur->ix++;
      pPage = pCur->pPage;
    }

    /* Descend to the child node of the cell that the cursor currently 
    ** points at. This is the right-child if (iIdx==pPage->nCell).
    */
    iIdx = pCur->ix;
    if( iIdx==pPage->nCell ){
      rc = moveToChild(pCur, get4byte(&pPage->aData[pPage->hdrOffset+8]));
    }else{
      rc = moveToChild(pCur, get4byte(findCell(pPage, iIdx)));
    }
  }

  /* An error has occurred. Return an error code. */
  return rc;
}

/*
** Return the pager associated with a BTree.  This routine is used for
** testing and debugging only.
*/
Pager *sqlite3BtreePager(Btree *p){
  return p->pBt->pPager;
}

#ifndef SQLITE_OMIT_INTEGRITY_CHECK
/*
** Append a message to the error message string.
*/
static void checkAppendMsg(
  IntegrityCk *pCheck,
  const char *zFormat,
  ...
){
  va_list ap;
  if( !pCheck->mxErr ) return;
  pCheck->mxErr--;
  pCheck->nErr++;
  va_start(ap, zFormat);
  if( pCheck->errMsg.nChar ){
    sqlite3_str_append(&pCheck->errMsg, "\n", 1);
  }
  if( pCheck->zPfx ){
    sqlite3_str_appendf(&pCheck->errMsg, pCheck->zPfx, pCheck->v1, pCheck->v2);
  }
  sqlite3_str_vappendf(&pCheck->errMsg, zFormat, ap);
  va_end(ap);
  if( pCheck->errMsg.accError==SQLITE_NOMEM ){
    pCheck->bOomFault = 1;
  }
}
#endif /* SQLITE_OMIT_INTEGRITY_CHECK */

#ifndef SQLITE_OMIT_INTEGRITY_CHECK

/*
** Return non-zero if the bit in the IntegrityCk.aPgRef[] array that
** corresponds to page iPg is already set.
*/
static int getPageReferenced(IntegrityCk *pCheck, Pgno iPg){
  assert( iPg<=pCheck->nPage && sizeof(pCheck->aPgRef[0])==1 );
  return (pCheck->aPgRef[iPg/8] & (1 << (iPg & 0x07)));
}

/*
** Set the bit in the IntegrityCk.aPgRef[] array that corresponds to page iPg.
*/
static void setPageReferenced(IntegrityCk *pCheck, Pgno iPg){
  assert( iPg<=pCheck->nPage && sizeof(pCheck->aPgRef[0])==1 );
  pCheck->aPgRef[iPg/8] |= (1 << (iPg & 0x07));
}


/*
** Add 1 to the reference count for page iPage.  If this is the second
** reference to the page, add an error message to pCheck->zErrMsg.
** Return 1 if there are 2 or more references to the page and 0 if
** if this is the first reference to the page.
**
** Also check that the page number is in bounds.
*/
static int checkRef(IntegrityCk *pCheck, Pgno iPage){
  if( iPage>pCheck->nPage || iPage==0 ){
    checkAppendMsg(pCheck, "invalid page number %d", iPage);
    return 1;
  }
  if( getPageReferenced(pCheck, iPage) ){
    checkAppendMsg(pCheck, "2nd reference to page %d", iPage);
    return 1;
  }
  if( AtomicLoad(&pCheck->db->u1.isInterrupted) ) return 1;
  setPageReferenced(pCheck, iPage);
  return 0;
}

#ifndef SQLITE_OMIT_AUTOVACUUM
/*
** Check that the entry in the pointer-map for page iChild maps to 
** page iParent, pointer type ptrType. If not, append an error message
** to pCheck.
*/
static void checkPtrmap(
  IntegrityCk *pCheck,   /* Integrity check context */
  Pgno iChild,           /* Child page number */
  u8 eType,              /* Expected pointer map type */
  Pgno iParent           /* Expected pointer map parent page number */
){
  int rc;
  u8 ePtrmapType;
  Pgno iPtrmapParent;

  rc = ptrmapGet(pCheck->pBt, iChild, &ePtrmapType, &iPtrmapParent);
  if( rc!=SQLITE_OK ){
    if( rc==SQLITE_NOMEM || rc==SQLITE_IOERR_NOMEM ) pCheck->bOomFault = 1;
    checkAppendMsg(pCheck, "Failed to read ptrmap key=%d", iChild);
    return;
  }

  if( ePtrmapType!=eType || iPtrmapParent!=iParent ){
    checkAppendMsg(pCheck,
      "Bad ptr map entry key=%d expected=(%d,%d) got=(%d,%d)", 
      iChild, eType, iParent, ePtrmapType, iPtrmapParent);
  }
}
#endif

/*
** Check the integrity of the freelist or of an overflow page list.
** Verify that the number of pages on the list is N.
*/
static void checkList(
  IntegrityCk *pCheck,  /* Integrity checking context */
  int isFreeList,       /* True for a freelist.  False for overflow page list */
  Pgno iPage,           /* Page number for first page in the list */
  u32 N                 /* Expected number of pages in the list */
){
  int i;
  u32 expected = N;
  int nErrAtStart = pCheck->nErr;
  while( iPage!=0 && pCheck->mxErr ){
    DbPage *pOvflPage;
    unsigned char *pOvflData;
    if( checkRef(pCheck, iPage) ) break;
    N--;
    if( sqlite3PagerGet(pCheck->pPager, (Pgno)iPage, &pOvflPage, 0) ){
      checkAppendMsg(pCheck, "failed to get page %d", iPage);
      break;
    }
    pOvflData = (unsigned char *)sqlite3PagerGetData(pOvflPage);
    if( isFreeList ){
      u32 n = (u32)get4byte(&pOvflData[4]);
#ifndef SQLITE_OMIT_AUTOVACUUM
      if( pCheck->pBt->autoVacuum ){
        checkPtrmap(pCheck, iPage, PTRMAP_FREEPAGE, 0);
      }
#endif
      if( n>pCheck->pBt->usableSize/4-2 ){
        checkAppendMsg(pCheck,
           "freelist leaf count too big on page %d", iPage);
        N--;
      }else{
        for(i=0; i<(int)n; i++){
          Pgno iFreePage = get4byte(&pOvflData[8+i*4]);
#ifndef SQLITE_OMIT_AUTOVACUUM
          if( pCheck->pBt->autoVacuum ){
            checkPtrmap(pCheck, iFreePage, PTRMAP_FREEPAGE, 0);
          }
#endif
          checkRef(pCheck, iFreePage);
        }
        N -= n;
      }
    }
#ifndef SQLITE_OMIT_AUTOVACUUM
    else{
      /* If this database supports auto-vacuum and iPage is not the last
      ** page in this overflow list, check that the pointer-map entry for
      ** the following page matches iPage.
      */
      if( pCheck->pBt->autoVacuum && N>0 ){
        i = get4byte(pOvflData);
        checkPtrmap(pCheck, i, PTRMAP_OVERFLOW2, iPage);
      }
    }
#endif
    iPage = get4byte(pOvflData);
    sqlite3PagerUnref(pOvflPage);
  }
  if( N && nErrAtStart==pCheck->nErr ){
    checkAppendMsg(pCheck,
      "%s is %d but should be %d",
      isFreeList ? "size" : "overflow list length",
      expected-N, expected);
  }
}
#endif /* SQLITE_OMIT_INTEGRITY_CHECK */

/*
** An implementation of a min-heap.
**
** aHeap[0] is the number of elements on the heap.  aHeap[1] is the
** root element.  The daughter nodes of aHeap[N] are aHeap[N*2]
** and aHeap[N*2+1].
**
** The heap property is this:  Every node is less than or equal to both
** of its daughter nodes.  A consequence of the heap property is that the
** root node aHeap[1] is always the minimum value currently in the heap.
**
** The btreeHeapInsert() routine inserts an unsigned 32-bit number onto
** the heap, preserving the heap property.  The btreeHeapPull() routine
** removes the root element from the heap (the minimum value in the heap)
** and then moves other nodes around as necessary to preserve the heap
** property.
**
** This heap is used for cell overlap and coverage testing.  Each u32
** entry represents the span of a cell or freeblock on a btree page.  
** The upper 16 bits are the index of the first byte of a range and the
** lower 16 bits are the index of the last byte of that range.
*/
static void btreeHeapInsert(u32 *aHeap, u32 x){
  u32 j, i = ++aHeap[0];
  aHeap[i] = x;
  while( (j = i/2)>0 && aHeap[j]>aHeap[i] ){
    x = aHeap[j];
    aHeap[j] = aHeap[i];
    aHeap[i] = x;
    i = j;
  }
}
static int btreeHeapPull(u32 *aHeap, u32 *pOut){
  u32 j, i, x;
  if( (x = aHeap[0])==0 ) return 0;
  *pOut = aHeap[1];
  aHeap[1] = aHeap[x];
  aHeap[x] = 0xffffffff;
  aHeap[0]--;
  i = 1;
  while( (j = i*2)<=aHeap[0] ){
    if( aHeap[j]>aHeap[j+1] ) j++;
    if( aHeap[i]<aHeap[j] ) break;
    x = aHeap[i];
    aHeap[i] = aHeap[j];
    aHeap[j] = x;
    i = j;
  }
  return 1;  
}

#ifndef SQLITE_OMIT_INTEGRITY_CHECK
/*
** Do various sanity checks on a single page of a tree.  Return
** the tree depth.  Root pages return 0.  Parents of root pages
** return 1, and so forth.
** 
** These checks are done:
**
**      1.  Make sure that cells and freeblocks do not overlap
**          but combine to completely cover the page.
**      2.  Make sure integer cell keys are in order.
**      3.  Check the integrity of overflow pages.
**      4.  Recursively call checkTreePage on all children.
**      5.  Verify that the depth of all children is the same.
*/
static int checkTreePage(
  IntegrityCk *pCheck,  /* Context for the sanity check */
  Pgno iPage,           /* Page number of the page to check */
  i64 *piMinKey,        /* Write minimum integer primary key here */
  i64 maxKey            /* Error if integer primary key greater than this */
){
  MemPage *pPage = 0;      /* The page being analyzed */
  int i;                   /* Loop counter */
  int rc;                  /* Result code from subroutine call */
  int depth = -1, d2;      /* Depth of a subtree */
  int pgno;                /* Page number */
  int nFrag;               /* Number of fragmented bytes on the page */
  int hdr;                 /* Offset to the page header */
  int cellStart;           /* Offset to the start of the cell pointer array */
  int nCell;               /* Number of cells */
  int doCoverageCheck = 1; /* True if cell coverage checking should be done */
  int keyCanBeEqual = 1;   /* True if IPK can be equal to maxKey
                           ** False if IPK must be strictly less than maxKey */
  u8 *data;                /* Page content */
  u8 *pCell;               /* Cell content */
  u8 *pCellIdx;            /* Next element of the cell pointer array */
  BtShared *pBt;           /* The BtShared object that owns pPage */
  u32 pc;                  /* Address of a cell */
  u32 usableSize;          /* Usable size of the page */
  u32 contentOffset;       /* Offset to the start of the cell content area */
  u32 *heap = 0;           /* Min-heap used for checking cell coverage */
  u32 x, prev = 0;         /* Next and previous entry on the min-heap */
  const char *saved_zPfx = pCheck->zPfx;
  int saved_v1 = pCheck->v1;
  int saved_v2 = pCheck->v2;
  u8 savedIsInit = 0;

  /* Check that the page exists
  */
  pBt = pCheck->pBt;
  usableSize = pBt->usableSize;
  if( iPage==0 ) return 0;
  if( checkRef(pCheck, iPage) ) return 0;
  pCheck->zPfx = "Page %u: ";
  pCheck->v1 = iPage;
  if( (rc = btreeGetPage(pBt, iPage, &pPage, 0))!=0 ){
    checkAppendMsg(pCheck,
       "unable to get the page. error code=%d", rc);
    goto end_of_check;
  }

  /* Clear MemPage.isInit to make sure the corruption detection code in
  ** btreeInitPage() is executed.  */
  savedIsInit = pPage->isInit;
  pPage->isInit = 0;
  if( (rc = btreeInitPage(pPage))!=0 ){
    assert( rc==SQLITE_CORRUPT );  /* The only possible error from InitPage */
    checkAppendMsg(pCheck,
                   "btreeInitPage() returns error code %d", rc);
    goto end_of_check;
  }
  if( (rc = btreeComputeFreeSpace(pPage))!=0 ){
    assert( rc==SQLITE_CORRUPT );
    checkAppendMsg(pCheck, "free space corruption", rc);
    goto end_of_check;
  }
  data = pPage->aData;
  hdr = pPage->hdrOffset;

  /* Set up for cell analysis */
  pCheck->zPfx = "On tree page %u cell %d: ";
  contentOffset = get2byteNotZero(&data[hdr+5]);
  assert( contentOffset<=usableSize );  /* Enforced by btreeInitPage() */

  /* EVIDENCE-OF: R-37002-32774 The two-byte integer at offset 3 gives the
  ** number of cells on the page. */
  nCell = get2byte(&data[hdr+3]);
  assert( pPage->nCell==nCell );

  /* EVIDENCE-OF: R-23882-45353 The cell pointer array of a b-tree page
  ** immediately follows the b-tree page header. */
  cellStart = hdr + 12 - 4*pPage->leaf;
  assert( pPage->aCellIdx==&data[cellStart] );
  pCellIdx = &data[cellStart + 2*(nCell-1)];

  if( !pPage->leaf ){
    /* Analyze the right-child page of internal pages */
    pgno = get4byte(&data[hdr+8]);
#ifndef SQLITE_OMIT_AUTOVACUUM
    if( pBt->autoVacuum ){
      pCheck->zPfx = "On page %u at right child: ";
      checkPtrmap(pCheck, pgno, PTRMAP_BTREE, iPage);
    }
#endif
    depth = checkTreePage(pCheck, pgno, &maxKey, maxKey);
    keyCanBeEqual = 0;
  }else{
    /* For leaf pages, the coverage check will occur in the same loop
    ** as the other cell checks, so initialize the heap.  */
    heap = pCheck->heap;
    heap[0] = 0;
  }

  /* EVIDENCE-OF: R-02776-14802 The cell pointer array consists of K 2-byte
  ** integer offsets to the cell contents. */
  for(i=nCell-1; i>=0 && pCheck->mxErr; i--){
    CellInfo info;

    /* Check cell size */
    pCheck->v2 = i;
    assert( pCellIdx==&data[cellStart + i*2] );
    pc = get2byteAligned(pCellIdx);
    pCellIdx -= 2;
    if( pc<contentOffset || pc>usableSize-4 ){
      checkAppendMsg(pCheck, "Offset %d out of range %d..%d",
                             pc, contentOffset, usableSize-4);
      doCoverageCheck = 0;
      continue;
    }
    pCell = &data[pc];
    pPage->xParseCell(pPage, pCell, &info);
    if( pc+info.nSize>usableSize ){
      checkAppendMsg(pCheck, "Extends off end of page");
      doCoverageCheck = 0;
      continue;
    }

    /* Check for integer primary key out of range */
    if( pPage->intKey ){
      if( keyCanBeEqual ? (info.nKey > maxKey) : (info.nKey >= maxKey) ){
        checkAppendMsg(pCheck, "Rowid %lld out of order", info.nKey);
      }
      maxKey = info.nKey;
      keyCanBeEqual = 0;     /* Only the first key on the page may ==maxKey */
    }

    /* Check the content overflow list */
    if( info.nPayload>info.nLocal ){
      u32 nPage;       /* Number of pages on the overflow chain */
      Pgno pgnoOvfl;   /* First page of the overflow chain */
      assert( pc + info.nSize - 4 <= usableSize );
      nPage = (info.nPayload - info.nLocal + usableSize - 5)/(usableSize - 4);
      pgnoOvfl = get4byte(&pCell[info.nSize - 4]);
#ifndef SQLITE_OMIT_AUTOVACUUM
      if( pBt->autoVacuum ){
        checkPtrmap(pCheck, pgnoOvfl, PTRMAP_OVERFLOW1, iPage);
      }
#endif
      checkList(pCheck, 0, pgnoOvfl, nPage);
    }

    if( !pPage->leaf ){
      /* Check sanity of left child page for internal pages */
      pgno = get4byte(pCell);
#ifndef SQLITE_OMIT_AUTOVACUUM
      if( pBt->autoVacuum ){
        checkPtrmap(pCheck, pgno, PTRMAP_BTREE, iPage);
      }
#endif
      d2 = checkTreePage(pCheck, pgno, &maxKey, maxKey);
      keyCanBeEqual = 0;
      if( d2!=depth ){
        checkAppendMsg(pCheck, "Child page depth differs");
        depth = d2;
      }
    }else{
      /* Populate the coverage-checking heap for leaf pages */
      btreeHeapInsert(heap, (pc<<16)|(pc+info.nSize-1));
    }
  }
  *piMinKey = maxKey;

  /* Check for complete coverage of the page
  */
  pCheck->zPfx = 0;
  if( doCoverageCheck && pCheck->mxErr>0 ){
    /* For leaf pages, the min-heap has already been initialized and the
    ** cells have already been inserted.  But for internal pages, that has
    ** not yet been done, so do it now */
    if( !pPage->leaf ){
      heap = pCheck->heap;
      heap[0] = 0;
      for(i=nCell-1; i>=0; i--){
        u32 size;
        pc = get2byteAligned(&data[cellStart+i*2]);
        size = pPage->xCellSize(pPage, &data[pc]);
        btreeHeapInsert(heap, (pc<<16)|(pc+size-1));
      }
    }
    /* Add the freeblocks to the min-heap
    **
    ** EVIDENCE-OF: R-20690-50594 The second field of the b-tree page header
    ** is the offset of the first freeblock, or zero if there are no
    ** freeblocks on the page. 
    */
    i = get2byte(&data[hdr+1]);
    while( i>0 ){
      int size, j;
      assert( (u32)i<=usableSize-4 ); /* Enforced by btreeComputeFreeSpace() */
      size = get2byte(&data[i+2]);
      assert( (u32)(i+size)<=usableSize ); /* due to btreeComputeFreeSpace() */
      btreeHeapInsert(heap, (((u32)i)<<16)|(i+size-1));
      /* EVIDENCE-OF: R-58208-19414 The first 2 bytes of a freeblock are a
      ** big-endian integer which is the offset in the b-tree page of the next
      ** freeblock in the chain, or zero if the freeblock is the last on the
      ** chain. */
      j = get2byte(&data[i]);
      /* EVIDENCE-OF: R-06866-39125 Freeblocks are always connected in order of
      ** increasing offset. */
      assert( j==0 || j>i+size );     /* Enforced by btreeComputeFreeSpace() */
      assert( (u32)j<=usableSize-4 ); /* Enforced by btreeComputeFreeSpace() */
      i = j;
    }
    /* Analyze the min-heap looking for overlap between cells and/or 
    ** freeblocks, and counting the number of untracked bytes in nFrag.
    ** 
    ** Each min-heap entry is of the form:    (start_address<<16)|end_address.
    ** There is an implied first entry the covers the page header, the cell
    ** pointer index, and the gap between the cell pointer index and the start
    ** of cell content.  
    **
    ** The loop below pulls entries from the min-heap in order and compares
    ** the start_address against the previous end_address.  If there is an
    ** overlap, that means bytes are used multiple times.  If there is a gap,
    ** that gap is added to the fragmentation count.
    */
    nFrag = 0;
    prev = contentOffset - 1;   /* Implied first min-heap entry */
    while( btreeHeapPull(heap,&x) ){
      if( (prev&0xffff)>=(x>>16) ){
        checkAppendMsg(pCheck,
          "Multiple uses for byte %u of page %u", x>>16, iPage);
        break;
      }else{
        nFrag += (x>>16) - (prev&0xffff) - 1;
        prev = x;
      }
    }
    nFrag += usableSize - (prev&0xffff) - 1;
    /* EVIDENCE-OF: R-43263-13491 The total number of bytes in all fragments
    ** is stored in the fifth field of the b-tree page header.
    ** EVIDENCE-OF: R-07161-27322 The one-byte integer at offset 7 gives the
    ** number of fragmented free bytes within the cell content area.
    */
    if( heap[0]==0 && nFrag!=data[hdr+7] ){
      checkAppendMsg(pCheck,
          "Fragmentation of %d bytes reported as %d on page %u",
          nFrag, data[hdr+7], iPage);
    }
  }

end_of_check:
  if( !doCoverageCheck ) pPage->isInit = savedIsInit;
  releasePage(pPage);
  pCheck->zPfx = saved_zPfx;
  pCheck->v1 = saved_v1;
  pCheck->v2 = saved_v2;
  return depth+1;
}
#endif /* SQLITE_OMIT_INTEGRITY_CHECK */

#ifndef SQLITE_OMIT_INTEGRITY_CHECK
/*
** This routine does a complete check of the given BTree file.  aRoot[] is
** an array of pages numbers were each page number is the root page of
** a table.  nRoot is the number of entries in aRoot.
**
** A read-only or read-write transaction must be opened before calling
** this function.
**
** Write the number of error seen in *pnErr.  Except for some memory
** allocation errors,  an error message held in memory obtained from
** malloc is returned if *pnErr is non-zero.  If *pnErr==0 then NULL is
** returned.  If a memory allocation error occurs, NULL is returned.
**
** If the first entry in aRoot[] is 0, that indicates that the list of
** root pages is incomplete.  This is a "partial integrity-check".  This
** happens when performing an integrity check on a single table.  The
** zero is skipped, of course.  But in addition, the freelist checks
** and the checks to make sure every page is referenced are also skipped,
** since obviously it is not possible to know which pages are covered by
** the unverified btrees.  Except, if aRoot[1] is 1, then the freelist
** checks are still performed.
*/
char *sqlite3BtreeIntegrityCheck(
  sqlite3 *db,  /* Database connection that is running the check */
  Btree *p,     /* The btree to be checked */
  Pgno *aRoot,  /* An array of root pages numbers for individual trees */
  int nRoot,    /* Number of entries in aRoot[] */
  int mxErr,    /* Stop reporting errors after this many */
  int *pnErr    /* Write number of errors seen to this variable */
){
  Pgno i;
  IntegrityCk sCheck;
  BtShared *pBt = p->pBt;
  u64 savedDbFlags = pBt->db->flags;
  char zErr[100];
  int bPartial = 0;            /* True if not checking all btrees */
  int bCkFreelist = 1;         /* True to scan the freelist */
  VVA_ONLY( int nRef );
  assert( nRoot>0 );

  /* aRoot[0]==0 means this is a partial check */
  if( aRoot[0]==0 ){
    assert( nRoot>1 );
    bPartial = 1;
    if( aRoot[1]!=1 ) bCkFreelist = 0;
  }

  sqlite3BtreeEnter(p);
  assert( p->inTrans>TRANS_NONE && pBt->inTransaction>TRANS_NONE );
  VVA_ONLY( nRef = sqlite3PagerRefcount(pBt->pPager) );
  assert( nRef>=0 );
  sCheck.db = db;
  sCheck.pBt = pBt;
  sCheck.pPager = pBt->pPager;
  sCheck.nPage = btreePagecount(sCheck.pBt);
  sCheck.mxErr = mxErr;
  sCheck.nErr = 0;
  sCheck.bOomFault = 0;
  sCheck.zPfx = 0;
  sCheck.v1 = 0;
  sCheck.v2 = 0;
  sCheck.aPgRef = 0;
  sCheck.heap = 0;
  sqlite3StrAccumInit(&sCheck.errMsg, 0, zErr, sizeof(zErr), SQLITE_MAX_LENGTH);
  sCheck.errMsg.printfFlags = SQLITE_PRINTF_INTERNAL;
  if( sCheck.nPage==0 ){
    goto integrity_ck_cleanup;
  }

  sCheck.aPgRef = sqlite3MallocZero((sCheck.nPage / 8)+ 1);
  if( !sCheck.aPgRef ){
    sCheck.bOomFault = 1;
    goto integrity_ck_cleanup;
  }
  sCheck.heap = (u32*)sqlite3PageMalloc( pBt->pageSize );
  if( sCheck.heap==0 ){
    sCheck.bOomFault = 1;
    goto integrity_ck_cleanup;
  }

  i = PENDING_BYTE_PAGE(pBt);
  if( i<=sCheck.nPage ) setPageReferenced(&sCheck, i);

  /* Check the integrity of the freelist
  */
  if( bCkFreelist ){
    sCheck.zPfx = "Main freelist: ";
    checkList(&sCheck, 1, get4byte(&pBt->pPage1->aData[32]),
              get4byte(&pBt->pPage1->aData[36]));
    sCheck.zPfx = 0;
  }

  /* Check all the tables.
  */
#ifndef SQLITE_OMIT_AUTOVACUUM
  if( !bPartial ){
    if( pBt->autoVacuum ){
      Pgno mx = 0;
      Pgno mxInHdr;
      for(i=0; (int)i<nRoot; i++) if( mx<aRoot[i] ) mx = aRoot[i];
      mxInHdr = get4byte(&pBt->pPage1->aData[52]);
      if( mx!=mxInHdr ){
        checkAppendMsg(&sCheck,
          "max rootpage (%d) disagrees with header (%d)",
          mx, mxInHdr
        );
      }
    }else if( get4byte(&pBt->pPage1->aData[64])!=0 ){
      checkAppendMsg(&sCheck,
        "incremental_vacuum enabled with a max rootpage of zero"
      );
    }
  }
#endif
  testcase( pBt->db->flags & SQLITE_CellSizeCk );
  pBt->db->flags &= ~(u64)SQLITE_CellSizeCk;
  for(i=0; (int)i<nRoot && sCheck.mxErr; i++){
    i64 notUsed;
    if( aRoot[i]==0 ) continue;
#ifndef SQLITE_OMIT_AUTOVACUUM
    if( pBt->autoVacuum && aRoot[i]>1 && !bPartial ){
      checkPtrmap(&sCheck, aRoot[i], PTRMAP_ROOTPAGE, 0);
    }
#endif
    checkTreePage(&sCheck, aRoot[i], &notUsed, LARGEST_INT64);
  }
  pBt->db->flags = savedDbFlags;

  /* Make sure every page in the file is referenced
  */
  if( !bPartial ){
    for(i=1; i<=sCheck.nPage && sCheck.mxErr; i++){
#ifdef SQLITE_OMIT_AUTOVACUUM
      if( getPageReferenced(&sCheck, i)==0 ){
        checkAppendMsg(&sCheck, "Page %d is never used", i);
      }
#else
      /* If the database supports auto-vacuum, make sure no tables contain
      ** references to pointer-map pages.
      */
      if( getPageReferenced(&sCheck, i)==0 && 
         (PTRMAP_PAGENO(pBt, i)!=i || !pBt->autoVacuum) ){
        checkAppendMsg(&sCheck, "Page %d is never used", i);
      }
      if( getPageReferenced(&sCheck, i)!=0 && 
         (PTRMAP_PAGENO(pBt, i)==i && pBt->autoVacuum) ){
        checkAppendMsg(&sCheck, "Pointer map page %d is referenced", i);
      }
#endif
    }
  }

  /* Clean  up and report errors.
  */
integrity_ck_cleanup:
  sqlite3PageFree(sCheck.heap);
  sqlite3_free(sCheck.aPgRef);
  if( sCheck.bOomFault ){
    sqlite3_str_reset(&sCheck.errMsg);
    sCheck.nErr++;
  }
  *pnErr = sCheck.nErr;
  if( sCheck.nErr==0 ) sqlite3_str_reset(&sCheck.errMsg);
  /* Make sure this analysis did not leave any unref() pages. */
  assert( nRef==sqlite3PagerRefcount(pBt->pPager) );
  sqlite3BtreeLeave(p);
  return sqlite3StrAccumFinish(&sCheck.errMsg);
}
#endif /* SQLITE_OMIT_INTEGRITY_CHECK */

/*
** Return the full pathname of the underlying database file.  Return
** an empty string if the database is in-memory or a TEMP database.
**
** The pager filename is invariant as long as the pager is
** open so it is safe to access without the BtShared mutex.
*/
const char *sqlite3BtreeGetFilename(Btree *p){
  assert( p->pBt->pPager!=0 );
  return sqlite3PagerFilename(p->pBt->pPager, 1);
}

/*
** Return the pathname of the journal file for this database. The return
** value of this routine is the same regardless of whether the journal file
** has been created or not.
**
** The pager journal filename is invariant as long as the pager is
** open so it is safe to access without the BtShared mutex.
*/
const char *sqlite3BtreeGetJournalname(Btree *p){
  assert( p->pBt->pPager!=0 );
  return sqlite3PagerJournalname(p->pBt->pPager);
}

/*
** Return one of SQLITE_TXN_NONE, SQLITE_TXN_READ, or SQLITE_TXN_WRITE
** to describe the current transaction state of Btree p.
*/
int sqlite3BtreeTxnState(Btree *p){
  assert( p==0 || sqlite3_mutex_held(p->db->mutex) );
  return p ? p->inTrans : 0;
}

#ifndef SQLITE_OMIT_WAL
/*
** Run a checkpoint on the Btree passed as the first argument.
**
** Return SQLITE_LOCKED if this or any other connection has an open 
** transaction on the shared-cache the argument Btree is connected to.
**
** Parameter eMode is one of SQLITE_CHECKPOINT_PASSIVE, FULL or RESTART.
*/
int sqlite3BtreeCheckpoint(Btree *p, int eMode, int *pnLog, int *pnCkpt){
  int rc = SQLITE_OK;
  if( p ){
    BtShared *pBt = p->pBt;
    sqlite3BtreeEnter(p);
    if( pBt->inTransaction!=TRANS_NONE ){
      rc = SQLITE_LOCKED;
    }else{
      rc = sqlite3PagerCheckpoint(pBt->pPager, p->db, eMode, pnLog, pnCkpt);
    }
    sqlite3BtreeLeave(p);
  }
  return rc;
}
#endif

/*
** Return true if there is currently a backup running on Btree p.
*/
int sqlite3BtreeIsInBackup(Btree *p){
  assert( p );
  assert( sqlite3_mutex_held(p->db->mutex) );
  return p->nBackup!=0;
}

/*
** This function returns a pointer to a blob of memory associated with
** a single shared-btree. The memory is used by client code for its own
** purposes (for example, to store a high-level schema associated with 
** the shared-btree). The btree layer manages reference counting issues.
**
** The first time this is called on a shared-btree, nBytes bytes of memory
** are allocated, zeroed, and returned to the caller. For each subsequent 
** call the nBytes parameter is ignored and a pointer to the same blob
** of memory returned. 
**
** If the nBytes parameter is 0 and the blob of memory has not yet been
** allocated, a null pointer is returned. If the blob has already been
** allocated, it is returned as normal.
**
** Just before the shared-btree is closed, the function passed as the 
** xFree argument when the memory allocation was made is invoked on the 
** blob of allocated memory. The xFree function should not call sqlite3_free()
** on the memory, the btree layer does that.
*/
void *sqlite3BtreeSchema(Btree *p, int nBytes, void(*xFree)(void *)){
  BtShared *pBt = p->pBt;
  sqlite3BtreeEnter(p);
  if( !pBt->pSchema && nBytes ){
    pBt->pSchema = sqlite3DbMallocZero(0, nBytes);
    pBt->xFreeSchema = xFree;
  }
  sqlite3BtreeLeave(p);
  return pBt->pSchema;
}

/*
** Return SQLITE_LOCKED_SHAREDCACHE if another user of the same shared 
** btree as the argument handle holds an exclusive lock on the 
** sqlite_schema table. Otherwise SQLITE_OK.
*/
int sqlite3BtreeSchemaLocked(Btree *p){
  int rc;
  assert( sqlite3_mutex_held(p->db->mutex) );
  sqlite3BtreeEnter(p);
  rc = querySharedCacheTableLock(p, SCHEMA_ROOT, READ_LOCK);
  assert( rc==SQLITE_OK || rc==SQLITE_LOCKED_SHAREDCACHE );
  sqlite3BtreeLeave(p);
  return rc;
}


#ifndef SQLITE_OMIT_SHARED_CACHE
/*
** Obtain a lock on the table whose root page is iTab.  The
** lock is a write lock if isWritelock is true or a read lock
** if it is false.
*/
int sqlite3BtreeLockTable(Btree *p, int iTab, u8 isWriteLock){
  int rc = SQLITE_OK;
  assert( p->inTrans!=TRANS_NONE );
  if( p->sharable ){
    u8 lockType = READ_LOCK + isWriteLock;
    assert( READ_LOCK+1==WRITE_LOCK );
    assert( isWriteLock==0 || isWriteLock==1 );

    sqlite3BtreeEnter(p);
    rc = querySharedCacheTableLock(p, iTab, lockType);
    if( rc==SQLITE_OK ){
      rc = setSharedCacheTableLock(p, iTab, lockType);
    }
    sqlite3BtreeLeave(p);
  }
  return rc;
}
#endif

#ifndef SQLITE_OMIT_INCRBLOB
/*
** Argument pCsr must be a cursor opened for writing on an 
** INTKEY table currently pointing at a valid table entry. 
** This function modifies the data stored as part of that entry.
**
** Only the data content may only be modified, it is not possible to 
** change the length of the data stored. If this function is called with
** parameters that attempt to write past the end of the existing data,
** no modifications are made and SQLITE_CORRUPT is returned.
*/
int sqlite3BtreePutData(BtCursor *pCsr, u32 offset, u32 amt, void *z){
  int rc;
  assert( cursorOwnsBtShared(pCsr) );
  assert( sqlite3_mutex_held(pCsr->pBtree->db->mutex) );
  assert( pCsr->curFlags & BTCF_Incrblob );

  rc = restoreCursorPosition(pCsr);
  if( rc!=SQLITE_OK ){
    return rc;
  }
  assert( pCsr->eState!=CURSOR_REQUIRESEEK );
  if( pCsr->eState!=CURSOR_VALID ){
    return SQLITE_ABORT;
  }

  /* Save the positions of all other cursors open on this table. This is
  ** required in case any of them are holding references to an xFetch
  ** version of the b-tree page modified by the accessPayload call below.
  **
  ** Note that pCsr must be open on a INTKEY table and saveCursorPosition()
  ** and hence saveAllCursors() cannot fail on a BTREE_INTKEY table, hence
  ** saveAllCursors can only return SQLITE_OK.
  */
  VVA_ONLY(rc =) saveAllCursors(pCsr->pBt, pCsr->pgnoRoot, pCsr);
  assert( rc==SQLITE_OK );

  /* Check some assumptions: 
  **   (a) the cursor is open for writing,
  **   (b) there is a read/write transaction open,
  **   (c) the connection holds a write-lock on the table (if required),
  **   (d) there are no conflicting read-locks, and
  **   (e) the cursor points at a valid row of an intKey table.
  */
  if( (pCsr->curFlags & BTCF_WriteFlag)==0 ){
    return SQLITE_READONLY;
  }
  assert( (pCsr->pBt->btsFlags & BTS_READ_ONLY)==0
              && pCsr->pBt->inTransaction==TRANS_WRITE );
  assert( hasSharedCacheTableLock(pCsr->pBtree, pCsr->pgnoRoot, 0, 2) );
  assert( !hasReadConflicts(pCsr->pBtree, pCsr->pgnoRoot) );
  assert( pCsr->pPage->intKey );

  return accessPayload(pCsr, offset, amt, (unsigned char *)z, 1);
}

/* 
** Mark this cursor as an incremental blob cursor.
*/
void sqlite3BtreeIncrblobCursor(BtCursor *pCur){
  pCur->curFlags |= BTCF_Incrblob;
  pCur->pBtree->hasIncrblobCur = 1;
}
#endif

/*
** Set both the "read version" (single byte at byte offset 18) and 
** "write version" (single byte at byte offset 19) fields in the database
** header to iVersion.
*/
int sqlite3BtreeSetVersion(Btree *pBtree, int iVersion){
  BtShared *pBt = pBtree->pBt;
  int rc;                         /* Return code */
 
  assert( iVersion==1 || iVersion==2 );

  /* If setting the version fields to 1, do not automatically open the
  ** WAL connection, even if the version fields are currently set to 2.
  */
  pBt->btsFlags &= ~BTS_NO_WAL;
  if( iVersion==1 ) pBt->btsFlags |= BTS_NO_WAL;

  rc = sqlite3BtreeBeginTrans(pBtree, 0, 0);
  if( rc==SQLITE_OK ){
    u8 *aData = pBt->pPage1->aData;
    if( aData[18]!=(u8)iVersion || aData[19]!=(u8)iVersion ){
      rc = sqlite3BtreeBeginTrans(pBtree, 2, 0);
      if( rc==SQLITE_OK ){
        rc = sqlite3PagerWrite(pBt->pPage1->pDbPage);
        if( rc==SQLITE_OK ){
          aData[18] = (u8)iVersion;
          aData[19] = (u8)iVersion;
        }
      }
    }
  }

  pBt->btsFlags &= ~BTS_NO_WAL;
  return rc;
}

/*
** Return true if the cursor has a hint specified.  This routine is
** only used from within assert() statements
*/
int sqlite3BtreeCursorHasHint(BtCursor *pCsr, unsigned int mask){
  return (pCsr->hints & mask)!=0;
}

/*
** Return true if the given Btree is read-only.
*/
int sqlite3BtreeIsReadonly(Btree *p){
  return (p->pBt->btsFlags & BTS_READ_ONLY)!=0;
}

/*
** Return the size of the header added to each page by this module.
*/
int sqlite3HeaderSizeBtree(void){ return ROUND8(sizeof(MemPage)); }

/*
** If no transaction is active and the database is not a temp-db, clear
** the in-memory pager cache.
*/
void sqlite3BtreeClearCache(Btree *p){
  BtShared *pBt = p->pBt;
  if( pBt->inTransaction==TRANS_NONE ){
    sqlite3PagerClearCache(pBt->pPager);
  }
}

#if !defined(SQLITE_OMIT_SHARED_CACHE)
/*
** Return true if the Btree passed as the only argument is sharable.
*/
int sqlite3BtreeSharable(Btree *p){
  return p->sharable;
}

/*
** Return the number of connections to the BtShared object accessed by
** the Btree handle passed as the only argument. For private caches 
** this is always 1. For shared caches it may be 1 or greater.
*/
int sqlite3BtreeConnectionCount(Btree *p){
  testcase( p->sharable );
  return p->pBt->nRef;
}
#endif
