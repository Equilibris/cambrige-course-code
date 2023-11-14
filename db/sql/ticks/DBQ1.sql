SELECT "name", "person_id", "birthyear" FROM people
WHERE  1950 <= birthyear 
AND    birthyear < 1960
ORDER BY name
LIMIT 250;
