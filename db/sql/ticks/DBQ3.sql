    -- mv_a.movie_id as mai, mv_b.movie_id as mbi, mv_c.movie_id as mci,
    -- rt_a.rating   as mar, rt_b.rating   as mbr, rt_c.rating    as mcr,
SELECT DISTINCT
    mv_a.title    as mat, mv_b.title    as mbt, mv_c.title    as mct,
    people.name as nm FROM people

INNER JOIN has_position hp_a ON people.person_id = hp_a.person_id
INNER JOIN has_position hp_b ON people.person_id = hp_b.person_id
INNER JOIN has_position hp_c ON people.person_id = hp_c.person_id

INNER JOIN movies mv_a on hp_a.movie_id = mv_a.movie_id
INNER JOIN movies mv_b on hp_b.movie_id = mv_b.movie_id
INNER JOIN movies mv_c on hp_c.movie_id = mv_c.movie_id

INNER JOIN ratings rt_a on rt_a.movie_id = mv_a.movie_id
INNER JOIN ratings rt_b on rt_b.movie_id = mv_b.movie_id
INNER JOIN ratings rt_c on rt_c.movie_id = mv_c.movie_id

WHERE 
    hp_a.position = "director"
AND hp_b.position = "director"
AND hp_c.position = "director"
AND mv_a.movie_id <> mv_b.movie_id
AND mv_a.movie_id <> mv_c.movie_id
AND mv_b.movie_id <> mv_c.movie_id
AND rt_a.votes < rt_b.votes
AND rt_b.votes < rt_c.votes
ORDER BY rt_a.votes, rt_b.votes, rt_c.votes DESC
LIMIT 250;
