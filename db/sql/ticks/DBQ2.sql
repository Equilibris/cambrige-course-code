SELECT name, role, COUNT(DISTINCT plays_role.movie_id) FROM people
INNER JOIN plays_role ON people.person_id = plays_role.person_id
GROUP BY people.person_id, plays_role.role
ORDER BY name, role
LIMIT 250

