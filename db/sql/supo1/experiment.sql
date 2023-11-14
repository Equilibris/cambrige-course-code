CREATE TABLE IF NOT EXISTS inst (
    id      INT     PRIMARY KEY,
    name    TEXT    NOT NULL
);

CREATE TABLE IF NOT EXISTS experiment (
    id      INT     PRIMARY KEY,
    iid     INT     NOT NULL,
    name    TEXT    NOT NULL,
    desc    TEXT    NOT NULL,
    typo_id INT     NOT NULL

    FOREIGN KEY(iid)        REFERENCES inst(id)
    FOREIGN KEY(typo_id)    REFERENCES typo(id)
);

CREATE TABLE IF NOT EXISTS typo (
    id      INT     PRIMARY KEY,
    data    TEXT    NOT NULL
    -- In a different DBMS this would be a distinct type
);

CREATE TABLE IF NOT EXISTS run (
    id      INT     PRIMARY KEY,
    eid     INT     NOT NULL,
    params  TEXT    NOT NULL

    FOREIGN KEY(eid)        REFERENCES experiment(id)
);

CREATE TABLE IF NOT EXISTS result (
    id      INT     PRIMARY KEY,
    rid     INT     NOT NULL,

    FOREIGN KEY(rid)        REFERENCES run(id)
);

SELECT
    json_extract(typo.data, "$.grid_width") AS width,
    json_extract(typo.data, "$.grid_height") AS height,
    AVG(json_extract(result.data, "$.message_count")) AS avg_count,
    AVG(json_extract(result.data, "$.run_time"))      AS avg_time
FROM experiment
INNER JOIN typo   ON typo.id = experiment.typo_id
INNER JOIN run    ON run.eid = experiment.id
INNER JOIN result ON run.id = result.rid
GROUP BY width, height;



