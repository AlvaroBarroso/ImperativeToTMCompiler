# PostgresQL training
## Presentation 1
### Locks
Use concurrently when *creating index* to minimize locking. Non-concurrently will lock inserts!. Concurrencly will take (much?) longer.

### Plans
- explain
- explain analyze
- explain verbose

- explain analyze + rollback? wtf?

if `cost` defers from `actual` by a lot, look at the statistics.

what's `width` in the plan? the length of the row in bytes.

Explain tools: `PgAdmin4` or https://explain.dalibo.com

### Vacuum
Everything is vacuum:
- Recover/reuse space.
- Update database statistics.
- Updates visibility map.
- Fix wraparound.

Index-only scans: visibility map avoids scanning the whole table to find out if a row is visible or not.

- vacuum:           only reclaims space
- vacuum analyze:   updates stats
- vacuum freeze:    performs aggresive freezing of tuples. Allows insert, update and deletes.
- vacuum full:      rebuilds the table (what table?) and replace the old one. It's slow and **locks** the table EVEN SELECTS. Defragmentation.
- vacuum verbose:   prints a detailed vacuum activity report for each table.

### Bloat
- Dead tuples: tuples that are not visible to any transaction.
`pgstattuple_approx` to get an approximation of the number of dead tuples.
`pgstattuple` to get the exact number of dead tuples.

`pg_repack` like `vacuum full` but without locking the table. Table must have a primary key or unique key. Similar to percona online schgeam change.
It duplicates the table, so you need the same amount of space until it finishes.


## Lab1

Last analyxe tinme:
```
select relname, last_analyze, last_autoanalyze 
from pg_stat_all_tables where relname like 'pgbench%';
``` 

dalibo.com:
Run following explain statement in the shell and paste the output to the "PLAN" textbox on the website.

```
psql -XqAt service=rdspg <<EOD
EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON)
select a.aid,a.abalance,count(*) hcount 
from pgbench_accounts a join pgbench_history h on a.aid=h.aid 
where h.mtime>current_timestamp-interval '1 day'
group by a.aid,a.abalance order by hcount desc;
EOD
```

## Presentation 2
### Tunning your workload with Indexes
#### B-tree
`create index name on table using btree (column);`
- Default index type. No need to say it explicitly.
- Good for equality and range queries.
- includes null values!

#### Hash
`create index name on table using hash (column);` first hash then bucket.
- `32-bit`
- Good for equality queries. Cannot range queries.
- size of index is small and inserts are faster than btree in big tables.
- optimal for joins

#### GiST (Generalized Search Tree)
`create index name on table using gist (column);`?
supports different data types:
    - geometric data types
    - trigram search
    - full text search
    - network address
    - range types
    ...

#### GIN (Generalized Inverted Index)
`create index name on table using gin (column);`?
- supports full text search. Duplicate key is great for this.
- JSONB data type
- Array
- Range
- suitable for indexing columns composite types.

#### GIST vs GIN
- GIN is about 3x faster for lookups than gist
- GIN is about 3x longer to build than gist
- GIN is about 10x slower to update than gist±
- GIN is about 2-3x smaller on disk than gist±

#### SP-GiST (Space Partitioned Generalized Search Tree)
Space mean a "value domain"
index not use btree implementation.
suitable for structures where the space can be recursively divided into subspaces. 
- search url

#### BRIN (Block Range INdex)
`create index name on table using brin (column);`?
it stores summaries for a range of blocks
uses less storage
tyny indexes designed for large tables
ideal for natural ordered table: timestaps or temperature sensor data

### Index desing patters
Covering index (index only scan!):

`create index account_idx_aid_balance on accounts(aid) include (balance)` makes `select aid, balance where aid=1;` faster.
data on the index is already sorted, so it's faster when tthere's a matching orderby.

Multi-column index:
`create index account_idx_aid_balance on accounts(aid, balance);` makes `select aid, balance where aid=1 and balance=2;` faster.

Functional index:
`create index account_idx_balance2 on accounts(abalance-10000);` makes `select aid, balance where abalance-10000>0;` faster.

Partial index:
also known as "filtering index".
`create index account_idx_balance3 on accounts(abalance) where abalance<>0;` makes `select aid, balance where abalance<>0;` faster.

### Index not being used
- When table is small or larger percentage of rows returned from the table.
- As a rule of thumb, using index usually is optimal for data fetched < 5% of the table total rows.
- Functions/Calculations around the column
- Data type mismatch: when such mismatch occurs, implicit data type conversion required. Same case as above.
- Skewed datasets: when you have skewed data and large percentage of rows returned Sqc scan wuth parallel will be more efficient than IndexScan.

### Join methods
It's up to the optimizer to define the best join method.
#### Nested Loop
Uses the first table (outer) as a driver, extract the data from the table and use each row to extract the data from the second table (inner). May need to have an index on the inner table.

#### Merge Join
Sorts both tables (in parallel), and merge them. Best with data pre-sorted through indexes.

#### Hash Join
Hashing the first table, haveing it on memory and the second is scanned using the hash table. Need enouth work_mem to hold these tables.

### Influence query plan
- Change optimizer parameters at the session level
- `set local enable_hashjoin=off;` or stuff like that.
- Creating new indexes
- Plan Hint: `extension pg_hint_plan` then `/** MergeJoin(a b) **/` before the query.

### Influence query plan using Aurora QPM

## Monitoring performance

Cloudwatch core metrics: (collected every minute):
- CPUUtilzation
- FreeLocalStorage
- DatabaseConnections
- FreeableMemory

Cloudwatch DB Engine metrics:
- Throughput
- Latency
- BufferCacheHitRatio
- ReplicationLag
- Deadlocks
- MaximumUsedTransactionIDs

Cloudwatch Enhanced monitoring (can collect every second) (needs to be enabled, seems expensive):
- memory.free
- cpuUtilization.*
- loadAverageMinute.*
- processList

RDS Performance Insights - Performance counters & waits:
- CPU
- IO
- Top wait events (engine specific)

### Using native postgresql views
Collected Statistics view (cumulative):
`pg_stat_database`
`pg_stat_user_tables` / `pg_stat_user_indexes`
`pg_statio_user_tables` / `pg_statio_user_indexes`

Collected Statistics view:
`pg_stat_activity`
`pg_stat_replication` / `pg_stat_subscription`
`pg_stat_progress_create_index` / `pg_stat_progress_vacuum`

### Extensions
`pg_stat_statements` - collects statistics about the queries.
`auto_explain` - logs the execution plan of the query.
`pg_proctab` - shows the os/process information as a table.