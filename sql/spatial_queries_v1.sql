-- Turtle Connectivity Ontario
-- 2024-02-03

-- Peter R. & Niamh Wall

-- Notes:
-- -To use this script you mus first install postgres+postgis database and import the data
-- -Here I create two spatial layers in R first then I imported them into Postgis

-- Queries to get unique site pair lines. This is useful so that we reduce the processing time
-- This are all Euclidean lines

SELECT gid2, site_pair2, geom, array_agg(site_pair1), array_agg(gid)
FROM
(
SELECT a.gid, a.site1||'to'||a.site2 AS site_pair1, b.site2||'to'||b.site1 AS site_pair2, b.gid AS gid2, b.geom
FROM tur_sites_lines_v1 AS a, tur_sites_lines_v1 AS b
WHERE ST_Equals(a.geom, b.geom) AND a.site1 <> b.site1
	) c
	GROUP BY gid2, site_pair2, geom
	--order by;
	
SELECT gid2, site_pair2, geom, array_agg(site_pair1), array_agg(gid)
FROM
(
SELECT a.gid, a.site1||'to'||a.site2 AS site_pair1, b.site1||'to'||b.site2 AS site_pair2, b.site2||'to'||b.site1 AS site_pair3, b.gid AS gid2, b.geom
FROM tur_sites_lines_v1 AS a, tur_sites_lines_v1 AS b
WHERE ST_Equals(a.geom, b.geom) AND a.site1 <> b.site1
	order by 1
	) c
	GROUP BY gid2, site_pair2, geom

CREATE OR REPLACE VIEW tur_unique_site_pair_lines_v1 AS
SELECT rid, agg_site_pair[1] AS site_pair1, agg_site_pair[2] AS site_pair2, geom::geometry('MultiLineString', 4326) AS geom
FROM
(SELECT row_number() over () as rid, array_agg(gid) agg_gid, array_agg(site1||'to'||site2) As agg_site_pair, geom
FROM tur_sites_lines_v1 
GROUP BY geom) t1;

SELECT * FROM tur_unique_site_pair_lines_v1;

-- DROP VIEW tur_unique_site_pair_buffers_v1
CREATE OR REPLACE VIEW tur_unique_site_pair_buffers_v2 AS
SELECT rid::smallint, site_pair1, site_pair2, st_transform(st_buffer(st_transform(geom, 3161), 500), 4326)
FROM tur_unique_site_pair_lines_v1;

SELECT * FROM tur_unique_site_pair_buffers_v2

-- Shortest path

CREATE OR REPLACE VIEW tur_shpath_buffers_1km_v1 AS

SELECT count(distinct geom)
FROM
(SELECT distinct rlyr, path_id, st_transform(st_buffer(st_transform(a.geom, 3161), 1000), 4326) as geom
FROM public.tur_shpath_v3 a
JOIN tur_unique_site_pair_lines_v1 b
ON a.path_id= b.site_pair1 --1424
UNION ALL
SELECT distinct rlyr, path_id, st_transform(st_buffer(st_transform(b.geom, 3161), 1000), 4326) as geom
FROM public.tur_shpath_v3 a
JOIN tur_unique_site_pair_lines_v1 b
ON a.path_id= b.site_pair2 ) c -- 1424)
; -- 1534. thus not all paths are the same.

-- Better just use:
CREATE OR REPLACE VIEW tur_shpath_buffers_1km_v1 AS
SELECT distinct rlyr, path_id, st_transform(st_buffer(st_transform(a.geom, 3161), 1000), 4326) as geom
FROM public.tur_shpath_v3 a -- 2848

--SELECT distinct rlyr, path_id, geom FROM public.tur_shpath_v3 -- 2722; 2848
