CREATE TABLE IF NOT EXISTS area (
    id      INT     PRIMARY KEY,
    name    TEXT    NOT NULL
);

CREATE TABLE IF NOT EXISTS crag (
    id      TEXT    PRIMARY KEY,
    aid     INT     NOT NULL,
    name    TEXT    NOT NULL,
    stone   TEXT    NOT NULL,
    FOREIGN KEY (aid)   REFERENCES AREA(id)
);

CREATE TABLE IF NOT EXISTS climb (
    id      INT     PRIMARY KEY,
    cid     INT     NOT NULL,
    name    TEXT    NOT NULL,
    length  INT     NOT NULL,
    difficulty TEXT NOT NULL,
    first_ascent DATE NOT NULL,
    original_ascent INT NOT NULL,
    FOREIGN KEY (cid)               REFERENCES crag(id),
    FOREIGN KEY (original_ascent)   REFERENCES climb(id)
);

CREATE TABLE IF NOT EXISTS climber (
    id      INT     PRIMARY KEY,
    name    TEXT    NOT NULL
);

CREATE TABLE IF NOT EXISTS guide (
    id      INT     PRIMARY KEY,
    name    TEXT    NOT NULL,
    pub     DATE    NOT NULL
);

CREATE TABLE IF NOT EXISTS guide_x_climb (
    id      INT     PRIMARY KEY,
    climb   INT     NOT NULL,
    guide   INT     NOT NULL,

    FOREIGN KEY (climb)     REFERENCES climb(id),
    FOREIGN KEY (guide)     REFERENCES guide(id)
);

CREATE TABLE IF NOT EXISTS ascent (
    id      INT     PRIMARY KEY,
    climb   INT     NOT NULL,
    climber INT     NOT NULL,
    date    DATE    NOT NULL,
    FOREIGN KEY (climb)     REFERENCES climb(id),
    FOREIGN KEY (climber)   REFERENCES climber(id)
);

SELECT id
FROM ascent
WHERE climber = (SELECT id FROM climber WHERE name = "J. Brown" LIMIT 1);

SELECT climb.name, climb.length
FROM climb
    INNER JOIN crag ON climb.cid = crag.id
    INNER JOIN area ON crag.aid  = area.id
WHERE
        area.name  = "Snowdonia"
    AND crag.stone = "slate"
    AND climb.difficulty = "hard severe";

SELECT name
FROM climb
WHERE first_ascent > (
    SELECT pub
    FROM guide
    WHERE name = "Climbers' Guide Sheffield and Strange End"
    ORDER BY pub DESC
    LIMIT 1
);

