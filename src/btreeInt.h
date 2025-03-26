/*

每个文件页包含 N 个数据库条目 和 N+1 个子页指针
----------------------------------------------------------------
|  Ptr(0) | Key(0) | Ptr(1) | Key(1) | ... | Key(N-1) | Ptr(N) |
----------------------------------------------------------------
Ptr(N) 指向的页的上面的所有键值在 Key(N-1) 和 Key(N+1) 之间
查找键值需读取 O(log(M)) 个页（M 为总条目数）

单个文件可包含多个独立 B-tree，通过 根页索引（Root Page Index） 标识。

负载（Payload）：键（Key）与数据（Data）组合成的存储单元。
溢出页（Overflow Pages）：若负载超过预设阈值，超出部分存入溢出页。
单元格（Cell）：负载与前驱指针组合成的存储单元。
每个页有一个页头包含了Ptr（N）指针和其他信息（比如key和data的大小）

文件由多个页组成，页号从1开始。页大小是2的幂次方，[512, 65536]
页有四种类型：B-Tree页、空闲页、溢出页、指针映射页（pointer-map page）

页1一定是个B树页，前一百个字节包含了一个文件头，用于描述文件。格式如下

  OFFSET   SIZE    DESCRIPTION
    0       16     固定字符串: "SQLite format 3\000"
    16       2     页大小（单位为字节）.  (1 means 65536)
    18       1     文件格式的写入版本（通常为 1）
    19       1     文件格式的读取版本（必须 ≤ 写入版本）
    20       1     每页末尾预留的未使用空间字节数（用于未来扩展）。
    21       1     单个单元格最大内联负载比例（默认 64，即 25% 的页大小，最大值为255,64/255 = 25%）。
    22       1     单个单元格最小内联负载比例（默认 32）
    23       1     LEAFDATA BTREE叶子节点单元格最小负载比例（默认 32）。
    24       4     File change counter，文件修改计数器，每次数据库变更时递增（用于缓存失效）。
    28       4     保留字段（未来可能用于扩展）。
    32       4     空闲页链表的首页号（若无空闲页则为 0）。
    36       4     空闲页总数。
// 元数据，给高层模块使用，共60字节
    40       4     数据库模式（如表结构、索引）变更时递增，用于检测是否需要重新解析模式。
    44       4     模式层文件格式：存储模式结构的内部格式版本（与 SQLite 版本相关）。
    48       4     页面缓存大小：建议的页面缓存容量（通常由操作系统管理，默认值为 0）。
    52       4     最大根页号：用于自动清理（auto_vacuum/incremental_vacuum）模式，记录最大 B-tree 根页号。
    56       4     文本编码：1= UTF-8，2= UTF-16LE，3= UTF-16BE。
    60       4     用户版本号：由应用程序自定义使用（如标记数据库版本）。
    64       4     增量清理模式：标识是否启用增量清理（0=禁用，非零=启用）。
    68       4     应用标识符：4 字节自定义标识，用于标记数据库所属应用（如 Firefox 使用特定值）。
    72      20     保留未使用：20 字节预留空间，未来可能扩展。
    92       4     版本有效性标识：确保文件头版本与数据库内容一致。
    96       4     SQLite 版本号：生成该数据库的 SQLite 库版本（如 3.39.0 对应 3039000）。

整数为大端序（高位字节在前）
文件修改计数器：数据库变更时递增，通知其他进程刷新缓存。
内联负载比例：在标准 B-tree（非 LEAFDATA 表）中，单个单元格（Cell）可占用的 单页可用空间的最大比例
溢出页机制
  溢出条件：若单元格的负载超过最大内联比例，超出部分将存入溢出页（Overflow Pages）。
  溢出策略：分配溢出页后，尽可能多地将数据移至溢出页，但需保证单元格内联部分仍满足 最小内联比例。溢出页形成链表，每页（除最后一页）存储 页大小 - 4 字节 数据（最后 4 字节存储下一溢出页号）。

每个BTREE页由三部分组成：页头、单元格指针数组、单元格内容。页1有100字节数组在页头前面
**
|----------------|
| file header    |   100 bytes.  Page 1 only.
|----------------|
| page header    |   叶子节点占8个字节，内部节点占12个字节
|----------------|
| cell pointer   |   |  排好序的单元格指针数组（每个元素2个字节），向下增长
| array          |   |  
|                |   v
|----------------|
| unallocated    |
| space          |
|----------------|   ^  
| cell content   |   |  单元格内容，向上增长
| area           |   |  
|----------------|

页头格式
OFFSET   SIZE     DESCRIPTION
  0       1      标志位  0x01:intkey 键为整数，直接存储在单元格头部（无需单独负载区域）
                        0x02:zerodata 仅存储键（Key），无数据（Value）
                        0x04:leafdata 
                        0x08:leaf 叶子节点
  1       2      第一个空闲块的字节偏移量（指向空闲块链表的起点）
  3       2      本页有多少个单元格
  5       2      单元格内容区域的起始字节偏移量
  7       1      碎片化空闲字节总数（无法加入空闲块的碎片空间）
  8       4      右子节点页号（仅内部节点存在，叶子节点没有）

单元格指针数组
  位于页面头部之后，由多个 2 字节偏移量 组成，每个偏移量指向单元格内容区域中某个单元格的起始位置。指针按键的顺序排列，确保快速二分查找。
  系统会在指针数组末尾预留空闲空间，以便插入新单元格时无需立即整理碎片。
单元格内容区域
  从页面末尾向头部方向增长。

空闲块链：未使用的空间被组织成链表，每个空闲块至少 4 字节。第一个空闲块地址在页头。小于4字节的内存碎片不放到链表上，碎片总大小在头部offset 7处。
空闲块格式：
SIZE    DESCRIPTION
  2     下一个空闲块的偏移
  2     本空闲块的大小

单元格是可变长度的。单元格存储在页面末端的单元格内容区域中。指向单元格的指针位于紧接页面头部之后的单元格指针数组中。
单元格不一定是连续的或按顺序排列的，但单元格指针是连续且按顺序排列的。

单元格内容使用可变长度整数编码，用1-9个字节存储最大为8个字节整数，编码方式为
每个字节的最高位用来表述后续的字节是不是同一组数据，最高位数为1，则表示后续的字节数据是同一组数据，剩下的七个位为真实数据，将所有字节的剩下七位拼凑起来则为真实数据。
最后一个字节的8位全是真实数据
0x00                      becomes  0x00000000
0x7f                      becomes  0x0000007f
0x81 0x00                 becomes  0x00000080
0x82 0x00                 becomes  0x00000100
0x80 0x7f                 becomes  0x0000007f
0x81 0x91 0xd1 0xac 0x78  becomes  0x12345678
0x81 0x81 0x81 0x81 0x01  becomes  0x10204081

可变长度整数编码用于行ID（rowids）并保存B树单元格中键和数据的字节数。
单元格格式为
  SIZE    DESCRIPTION
    4     左子节点的页号. （叶子节点没有）
  可变   数据长度. zerodata页没有
  可变   key的长度. intkey页为key本身
    *     负载
    4     溢出页链表的第一个页，没有溢出页则没有

溢出页面形成一个链表。除了最后一页外，每页都完全填充了数据（页面大小 - 4字节）。最后一页可以只有1字节的数据。
格式如下
  SIZE    DESCRIPTION
    4     下一个溢出页的页号
    *     Data

空闲列表页面有两种子类型：主干页面（trunk pages）和叶子页面（leaf pages）。
文件头部指向主干页面链表中的第一个页面。每个主干页面指向多个叶子页面。叶子页面的内容未指定。主干页面的结构如下：
SIZE    DESCRIPTION
  4     下一个主干页面的页号
  4     本页的叶子页面数量
  *     零个或多个叶子页面的页面号
*/
#include "sqliteInt.h"


// 最大单元格大小
#define MX_CELL_SIZE(pBt)  ((int)(pBt->pageSize-8))

// 单页可存Cell数量的最大值，6 = 4 + 2，Cell长度最小值 4 + Cell偏移 2
#define MX_CELL(pBt) ((pBt->pageSize-8)/6)

typedef struct MemPage MemPage;
typedef struct BtLock BtLock;
typedef struct CellInfo CellInfo;

#ifndef SQLITE_FILE_HEADER /* 123456789 123456 */
#  define SQLITE_FILE_HEADER "SQLite format 3"
#endif

// B树页类型
#define PTF_INTKEY    0x01
#define PTF_ZERODATA  0x02
#define PTF_LEAFDATA  0x04
#define PTF_LEAF      0x08

/*
此对象的一个实例用于存储已加载到内存中的每个独立数据库页的元信息。信息来源于磁盘上的原始页数据解析。
当数据库页被加载到内存时，页管理器（pager）会分配此对象的实例，并将其前 8 字节初始化为零（这是与页管理器中每个页关联的“额外”信息）。
对此结构体所有字段的访问，均受 MemPage.pBt->mutex 互斥锁控制。
*/
struct MemPage {
  u8 isInit;           /* True if previously initialized. MUST BE FIRST! */
  u8 intKey;           /* True if table b-trees.  False for index b-trees */
  u8 intKeyLeaf;       /* True if the leaf of an intKey table */
  Pgno pgno;           /* Page number for this page */
  /* Only the first 8 bytes (above) are zeroed by pager.c when a new page
  ** is allocated. All fields that follow must be initialized before use */
  u8 leaf;             /* True if a leaf page */
  u8 hdrOffset;        /* 100 for page 1.  0 otherwise */
  u8 childPtrSize;     /* 0 if leaf==1.  4 if leaf==0 */
  u8 max1bytePayload;  /* min(maxLocal,127) */
  u8 nOverflow;        // 本页含有溢出页的Cell的个数
  u16 maxLocal;        /* Copy of BtShared.maxLocal or BtShared.maxLeaf */
  u16 minLocal;        /* Copy of BtShared.minLocal or BtShared.minLeaf */
  u16 cellOffset;      // 存储Cell数组位置的偏移
  int nFree;           /* Number of free bytes on the page. -1 for unknown */
  u16 nCell;           /* Number of cells on this page, local and ovfl */
  u16 maskPage;        /* Mask for page offset */
  u16 aiOvfl[4];       /* Insert the i-th overflow cell before the aiOvfl-th
                       ** non-overflow cell */
  u8 *apOvfl[4];       /* Pointers to the body of overflow cells */
  BtShared *pBt;       /* Pointer to BtShared that this page is part of */
  u8 *aData;           /* Pointer to disk image of the page data */
  u8 *aDataEnd;        // 指向整个页面末尾的下一个字节（不仅计算可用空间，而是整个页面的物理末尾）。用于防止因数据损坏导致的缓冲区溢出。
  u8 *aCellIdx;        /* The cell index area */
  u8 *aDataOfst;       /* Same as aData for leaves.  aData+4 for interior */
  DbPage *pDbPage;     /* Pager page handle */
  u16 (*xCellSize)(MemPage*,u8*);             /* cellSizePtr method */
  void (*xParseCell)(MemPage*,u8*,CellInfo*); /* btreeParseCell method */
};

/*
以下结构的链表存储在 BtShared.pLock 中。
当针对根页为 BtShared.iTable 的表开启游标时，锁会被添加（或从 READ_LOCK 升级为 WRITE_LOCK）。锁会在事务提交/回滚，或 B树句柄关闭时从该链表中移除。
*/
struct BtLock {
  Btree *pBtree;        /* Btree handle holding this lock */
  Pgno iTable;          /* Root page of table */
  u8 eLock;             /* READ_LOCK or WRITE_LOCK */
  BtLock *pNext;        /* Next in BtShared.pLock list */
};

/* Candidate values for BtLock.eLock */
#define READ_LOCK     1
#define WRITE_LOCK    2

/* A Btree handle
**
** A database connection contains a pointer to an instance of
** this object for every database file that it has open.  This structure
** is opaque to the database connection.  The database connection cannot
** see the internals of this structure and only deals with pointers to
** this structure.
**
** For some database files, the same underlying database cache might be 
** shared between multiple connections.  In that case, each connection
** has it own instance of this object.  But each instance of this object
** points to the same BtShared object.  The database cache and the
** schema associated with the database file are all contained within
** the BtShared object.
**
** All fields in this structure are accessed under sqlite3.mutex.
** The pBt pointer itself may not be changed while there exists cursors 
** in the referenced BtShared that point back to this Btree since those
** cursors have to go through this Btree to find their BtShared and
** they often do so without holding sqlite3.mutex.
*/
struct Btree {
  sqlite3 *db;       /* The database connection holding this btree */
  BtShared *pBt;     /* Sharable content of this btree */
  u8 inTrans;        /* TRANS_NONE, TRANS_READ or TRANS_WRITE */
  u8 sharable;       /* True if we can share pBt with another db */
  u8 locked;         /* True if db currently has pBt locked */
  u8 hasIncrblobCur; /* True if there are one or more Incrblob cursors */
  int wantToLock;    /* Number of nested calls to sqlite3BtreeEnter() */
  int nBackup;       /* Number of backup operations reading this btree */
  u32 iBDataVersion; /* Combines with pBt->pPager->iDataVersion */
  Btree *pNext;      /* List of other sharable Btrees from the same db */
  Btree *pPrev;      /* Back pointer of the same list */
#ifdef SQLITE_DEBUG
  u64 nSeek;         /* Calls to sqlite3BtreeMovetoUnpacked() */
#endif
#ifndef SQLITE_OMIT_SHARED_CACHE
  BtLock lock;       /* Object used to lock page 1 */
#endif
};

/*
** Btree.inTrans may take one of the following values.
**
** If the shared-data extension is enabled, there may be multiple users
** of the Btree structure. At most one of these may open a write transaction,
** but any number may have active read transactions.
**
** These values must match SQLITE_TXN_NONE, SQLITE_TXN_READ, and
** SQLITE_TXN_WRITE
*/
#define TRANS_NONE  0
#define TRANS_READ  1
#define TRANS_WRITE 2

#if TRANS_NONE!=SQLITE_TXN_NONE
# error wrong numeric code for no-transaction
#endif
#if TRANS_READ!=SQLITE_TXN_READ
# error wrong numeric code for read-transaction
#endif
#if TRANS_WRITE!=SQLITE_TXN_WRITE
# error wrong numeric code for write-transaction
#endif


/*
该对象的实例代表一个独立的数据库文件

同一数据库文件可被两个或多个数据库连接同时使用。当多个连接共享同一数据库文件时，
每个连接拥有自己的私有 Btree 对象（指向该文件），且所有这些 Btree 对象均指向
本 BtShared 对象。BtShared.nRef 表示当前共享此数据库文件的连接数。

此结构体中的字段默认通过 BtShared.mutex 互斥锁保护访问，但 nRef 和 pNext 字段
需在全局锁 SQLITE_MUTEX_STATIC_MAIN 下访问。pPager 字段在 nRef>0 时，
一旦初始化后不可修改。pSchema 字段可在 BtShared.mutex 下初始化一次，
之后在 nRef>0 时保持不变。

isPending（挂起锁状态标志）：
当 BtShared 的客户端（如连接）因存在读锁而无法获取某数据库表的写锁时，
共享缓存将进入'pending-lock'（挂起锁）状态，此时 isPending 被设为 true。
满足以下任一条件时，共享缓存退出'pending-lock'状态：

1) 当前写入者（BtShared.pWriter）完成事务，或
2) 其他连接持有的锁数量降为零

在'pending-lock'状态下，所有连接均不能启动新事务。
此机制旨在防止写操作饥饿（writer-starvation）问题。
*/
struct BtShared {
  Pager *pPager;        /* The page cache */
  sqlite3 *db;          /* Database connection currently using this Btree */
  BtCursor *pCursor;    /* A list of all open cursors */
  MemPage *pPage1;      /* First page of the database */
  u8 openFlags;         /* Flags to sqlite3BtreeOpen() */
#ifndef SQLITE_OMIT_AUTOVACUUM
  u8 autoVacuum;        /* True if auto-vacuum is enabled */
  u8 incrVacuum;        /* True if incr-vacuum is enabled */
  u8 bDoTruncate;       /* True to truncate db on commit */
#endif
  u8 inTransaction;     /* Transaction state */
  u8 max1bytePayload;   /* Maximum first byte of cell for a 1-byte payload */
  u8 nReserveWanted;    /* Desired number of extra bytes per page */
  u16 btsFlags;         /* Boolean parameters.  See BTS_* macros below */
  u16 maxLocal;         /* Maximum local payload in non-LEAFDATA tables */
  u16 minLocal;         /* Minimum local payload in non-LEAFDATA tables */
  u16 maxLeaf;          /* Maximum local payload in a LEAFDATA table */
  u16 minLeaf;          /* Minimum local payload in a LEAFDATA table */
  u32 pageSize;         /* Total number of bytes on a page */
  u32 usableSize;       /* Number of usable bytes on each page */
  int nTransaction;     /* Number of open transactions (read + write) */
  u32 nPage;            /* Number of pages in the database */
  void *pSchema;        /* Pointer to space allocated by sqlite3BtreeSchema() */
  void (*xFreeSchema)(void*);  /* Destructor for BtShared.pSchema */
  sqlite3_mutex *mutex; /* Non-recursive mutex required to access this object */
  Bitvec *pHasContent;  /* Set of pages moved to free-list this transaction */
#ifndef SQLITE_OMIT_SHARED_CACHE
  int nRef;             /* Number of references to this structure */
  BtShared *pNext;      /* Next on a list of sharable BtShared structs */
  BtLock *pLock;        /* List of locks held on this shared-btree struct */
  Btree *pWriter;       /* Btree with currently open write transaction */
#endif
  u8 *pTmpSpace;        /* Temp space sufficient to hold a single cell */
  int nPreformatSize;   /* Size of last cell written by TransferRow() */
};

/*
** Allowed values for BtShared.btsFlags
*/
#define BTS_READ_ONLY        0x0001   /* Underlying file is readonly */
#define BTS_PAGESIZE_FIXED   0x0002   /* Page size can no longer be changed */
#define BTS_SECURE_DELETE    0x0004   /* PRAGMA secure_delete is enabled */
#define BTS_OVERWRITE        0x0008   /* Overwrite deleted content with zeros */
#define BTS_FAST_SECURE      0x000c   /* Combination of the previous two */
#define BTS_INITIALLY_EMPTY  0x0010   /* Database was empty at trans start */
#define BTS_NO_WAL           0x0020   /* Do not open write-ahead-log files */
#define BTS_EXCLUSIVE        0x0040   /* pWriter has an exclusive lock */
#define BTS_PENDING          0x0080   /* Waiting for read-locks to clear */


// 以下结构体的实例用于存储单元格（cell）的元信息。parseCellPtr() 函数会基于从原始磁盘页提取的数据，填充此结构体。
struct CellInfo {
  i64 nKey;      /* The key for INTKEY tables, or nPayload otherwise */
  u8 *pPayload;  /* Pointer to the start of payload */
  u32 nPayload;  /* Bytes of payload */
  u16 nLocal;    // 记录在本页上的负载长度，不包含溢出页的
  u16 nSize;     // CellSize
};

/*
SQLite B-Tree 结构的最大深度限制。任何深度超过此值的 B-Tree 将被判定为损坏。
该深度值基于以下条件计算得出：
- 数据库最大尺寸为 2^31 页
- 根节点最小扇出为 2
- 其他内部节点最小扇出为 3

若检测到树层级超过此限制，则认定数据库存在损坏。
扇出（fanout）:B-Tree 节点可包含的子节点数量
*/
#define BTCURSOR_MAX_DEPTH 20

/*
游标（cursor）是数据库文件中特定 B 树内某个具体条目的定位指针，该条目通过其所在的 MemPage 内存页及页内单元格数组 MemPage.aCell[] 的索引共同标识。

单个数据库文件可被两个或多个数据库连接共享，但游标不可共享。每个游标关联到特定的数据库连接，由 BtCursor.pBtree.db 字段标识。

此结构体中的字段需在 self->pBt->mutex（即 BtShared.mutex）互斥锁保护下访问。

skipNext 字段含义（根据 eState 状态不同）：
  eState 状态        | skipNext 的含义
  ------------------|------------------------------------
  VALID（有效）      | skipNext 无意义，被忽略
  INVALID（无效）    | skipNext 无意义，被忽略
  SKIPNEXT（跳转）   | sqlite3BtreeNext() 在 skipNext>0 时无操作；sqlite3BtreePrevious() 在 skipNext<0 时无操作
  REQUIRESEEK（需重定位） restoreCursorPosition() 将游标恢复至eState=SKIPNEXT 状态（若 skipNext!=0）
  FAULT（错误）      | skipNext 存储游标的错误代码
*/
struct BtCursor {
  u8 eState;                /* One of the CURSOR_XXX constants (see below) */
  u8 curFlags;              /* zero or more BTCF_* flags defined below */
  u8 curPagerFlags;         /* Flags to send to sqlite3PagerGet() */
  u8 hints;                 /* As configured by CursorSetHints() */
  int skipNext;    /* Prev() is noop if negative. Next() is noop if positive.
                   ** Error code if eState==CURSOR_FAULT */
  Btree *pBtree;            /* The Btree to which this cursor belongs */
  Pgno *aOverflow;          /* Cache of overflow page locations */
  void *pKey;               /* Saved key that was cursor last known position */
  /* All fields above are zeroed when the cursor is allocated.  See
  ** sqlite3BtreeCursorZero().  Fields that follow must be manually
  ** initialized. */
#define BTCURSOR_FIRST_UNINIT pBt   /* Name of first uninitialized field */
  BtShared *pBt;            /* The BtShared this cursor points to */
  BtCursor *pNext;          /* Forms a linked list of all cursors */
  CellInfo info;            /* A parse of the cell we are pointing at */
  i64 nKey;                 /* Size of pKey, or last integer key */
  Pgno pgnoRoot;            /* The root page of this tree */
  i8 iPage;                 /* Index of current page in apPage */
  u8 curIntKey;             /* Value of apPage[0]->intKey */
  u16 ix;                   /* Current index for apPage[iPage] */
  u16 aiIdx[BTCURSOR_MAX_DEPTH-1];     /* Current index in apPage[i] */
  struct KeyInfo *pKeyInfo;            /* Arg passed to comparison function */
  MemPage *pPage;                        /* Current page */
  MemPage *apPage[BTCURSOR_MAX_DEPTH-1]; /* Stack of parents of current page */
};

/*
** Legal values for BtCursor.curFlags
*/
#define BTCF_WriteFlag    0x01   /* True if a write cursor */
#define BTCF_ValidNKey    0x02   /* True if info.nKey is valid */
#define BTCF_ValidOvfl    0x04   /* True if aOverflow is valid */
#define BTCF_AtLast       0x08   /* Cursor is pointing ot the last entry */
#define BTCF_Incrblob     0x10   /* True if an incremental I/O handle */
#define BTCF_Multiple     0x20   /* Maybe another cursor on the same btree */
#define BTCF_Pinned       0x40   /* Cursor is busy and cannot be moved */

/*
** BtCursor.eState 的可能取值说明
**
** CURSOR_INVALID：
** 游标未指向有效条目。可能因以下情况产生：
** - 数据表为空
** - 尚未调用 BtreeCursorFirst() 初始化
**
** CURSOR_VALID：
** 游标指向有效条目。此时可安全调用 getPayload() 等方法
**
** CURSOR_SKIPNEXT：
** 游标处于有效状态，但 Cursor.skipNext 字段非零，表示
** 接下来的 sqlite3BtreeNext() 或 sqlite3BtreePrevious()
** 操作应跳过执行（空操作）
**
** CURSOR_REQUIRESEEK：
** 游标所属数据表仍存在，但自上次使用后已被修改。游标位置 已保存在 BtCursor.pKey 和 BtCursor.nKey 变量中。处于此状态时，可调用 restoreCursorPosition() 尝试将游标恢复到保存的位置
**
** CURSOR_FAULT：
** 共享同一 BtShared 缓存的其他连接发生不可恢复错误(如 I/O 错误或内存分配失败)，导致缓存处于不一致状态。
** 该游标应立即停止使用，任何后续操作都应返回BtCursor.skipNext 中存储的错误代码
*/
#define CURSOR_VALID             0
#define CURSOR_INVALID           1
#define CURSOR_SKIPNEXT          2
#define CURSOR_REQUIRESEEK       3
#define CURSOR_FAULT             4

// PENDING_BYTE 所在的数据库页。该页从未被实际使用。
// SQLite 中一个特殊的字节位置（通常位于文件第 0x40000000 偏移处），用于实现数据库文件的 写锁机制（Pending Lock）。多个进程/线程通过原子操作竞争此字节的锁状态，以实现并发控制。
#define PENDING_BYTE_PAGE(pBt)  ((Pgno)((PENDING_BYTE/((pBt)->pageSize))+1))

/*
以下宏定义用于定位数据库页的指针映射条目（pointer-map entry）。
每个宏的第一个参数是数据库每页的可用字节数（通常为 1024），第二个参数是要查询指针映射的目标页号（pgno）。

PTRMAP_PAGENO：返回存储目标页指针映射条目的指针映射页的数据库页号。
PTRMAP_PTROFFSET：返回目标页指针映射条目在指针映射页内的偏移量。

若传递给 PTRMAP_PAGENO 的 pgno 本身是指针映射页，则直接返回 pgno。因此可通过 (pgno == PTRMAP_PAGENO(pgsz, pgno)) 判断 pgno 是否为指针映射页，PTRMAP_ISPAGE 宏实现了此判断逻辑。
*/
#define PTRMAP_PAGENO(pBt, pgno) ptrmapPageno(pBt, pgno)
#define PTRMAP_PTROFFSET(pgptrmap, pgno) (5*(pgno-pgptrmap-1))
#define PTRMAP_ISPAGE(pBt, pgno) (PTRMAP_PAGENO((pBt),(pgno))==(pgno))

/*
** 指针映射是一个查找表，用于记录数据库文件中每个子页的父页信息。父页即包含指向该子页指针的页。
** 每个数据库页（此处指非指针映射页）有且仅有 0 或 1 个父页。每个指针映射条目由 1 字节的'类型'
** 和 4 字节的父页号组成。下方 PTRMAP_XXX 标识符为有效类型定义。
**
** 指针映射的核心作用是为自动清理（AutoVacuum）过程中的页移动提供支持。当页被移动时，
** 其父页中的指针需更新为新位置。指针映射用于快速定位父页，确保引用一致性。
**
** 类型定义：
**   - PTRMAP_ROOTPAGE：该数据库页是 B-Tree 的根页，此时父页号字段无意义。
**   - PTRMAP_FREEPAGE：该页是空闲页（未被使用），父页号字段无意义。
**   - PTRMAP_OVERFLOW1：该页是溢出页链的首个页，父页号指向包含其引用的单元格所在页。
**   - PTRMAP_OVERFLOW2：该页是溢出页链的后续页，父页号指向前一个溢出页。
**   - PTRMAP_BTREE：该页是 B-Tree 的非根内部节点页，父页号指向其直接父节点页。
*/
#define PTRMAP_ROOTPAGE 1
#define PTRMAP_FREEPAGE 2
#define PTRMAP_OVERFLOW1 3
#define PTRMAP_OVERFLOW2 4
#define PTRMAP_BTREE 5


#define btreeIntegrity(p) \
  assert( p->pBt->inTransaction!=TRANS_NONE || p->pBt->nTransaction==0 ); \
  assert( p->pBt->inTransaction>=p->inTrans ); 

#ifndef SQLITE_OMIT_AUTOVACUUM
#define ISAUTOVACUUM(pBt) (pBt->autoVacuum)
#else
#define ISAUTOVACUUM(pBt) 0
#endif


/*
** This structure is passed around through all the sanity checking routines
** in order to keep track of some global state information.
**
** The aRef[] array is allocated so that there is 1 bit for each page in
** the database. As the integrity-check proceeds, for each page used in
** the database the corresponding bit is set. This allows integrity-check to 
** detect pages that are used twice and orphaned pages (both of which 
** indicate corruption).
*/
typedef struct IntegrityCk IntegrityCk;
struct IntegrityCk {
  BtShared *pBt;    /* The tree being checked out */
  Pager *pPager;    /* The associated pager.  Also accessible by pBt->pPager */
  u8 *aPgRef;       /* 1 bit per page in the db (see above) */
  Pgno nPage;       /* Number of pages in the database */
  int mxErr;        /* Stop accumulating errors when this reaches zero */
  int nErr;         /* Number of messages written to zErrMsg so far */
  int bOomFault;    /* A memory allocation error has occurred */
  const char *zPfx; /* Error message prefix */
  Pgno v1;          /* Value for first %u substitution in zPfx */
  int v2;           /* Value for second %d substitution in zPfx */
  StrAccum errMsg;  /* Accumulate the error message text here */
  u32 *heap;        /* Min-heap used for analyzing cell coverage */
  sqlite3 *db;      /* Database connection running the check */
};

/*
** Routines to read or write a two- and four-byte big-endian integer values.
*/
#define get2byte(x)   ((x)[0]<<8 | (x)[1])
#define put2byte(p,v) ((p)[0] = (u8)((v)>>8), (p)[1] = (u8)(v))
#define get4byte sqlite3Get4byte
#define put4byte sqlite3Put4byte

/*
** get2byteAligned(), unlike get2byte(), requires that its argument point to a
** two-byte aligned address.  get2bytea() is only used for accessing the
** cell addresses in a btree header.
*/
#if SQLITE_BYTEORDER==4321
# define get2byteAligned(x)  (*(u16*)(x))
#elif SQLITE_BYTEORDER==1234 && GCC_VERSION>=4008000
# define get2byteAligned(x)  __builtin_bswap16(*(u16*)(x))
#elif SQLITE_BYTEORDER==1234 && MSVC_VERSION>=1300
# define get2byteAligned(x)  _byteswap_ushort(*(u16*)(x))
#else
# define get2byteAligned(x)  ((x)[0]<<8 | (x)[1])
#endif
