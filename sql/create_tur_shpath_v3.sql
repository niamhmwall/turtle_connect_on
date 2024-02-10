CREATE TABLE IF NOT EXISTS public.tur_shpath_v3
(
    gid serial,
    site_from smallint,
    site_to smallint,
    rlyr smallint,
    rlyrlab character varying(13) COLLATE pg_catalog."default",
    site_from_ character varying(10) COLLATE pg_catalog."default",
    site_to_id character varying(10) COLLATE pg_catalog."default",
    path_id character varying(10) COLLATE pg_catalog."default",
    sp smallint,
    rid smallint,
    geom geometry(MultiLineString,3161),
    CONSTRAINT tur_shpath_v3_pkey PRIMARY KEY (gid)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.tur_shpath_v3
    OWNER to postgres;
-- Index: tur_shpath_v2_geom_idx

-- DROP INDEX IF EXISTS public.tur_shpath_v2_geom_idx;

CREATE INDEX IF NOT EXISTS tur_shpath_v3_geom_idx
    ON public.tur_shpath_v3 USING gist
    (geom)
    TABLESPACE pg_default;
	
INSERT INTO public.tur_shpath_v3
SELECT * FROM public.tur_shpath_v2


-- DROP VIEW public.tur_shpath_buffers_1km_v1;

CREATE OR REPLACE VIEW public.tur_shpath_buffers_1km_v1
 AS
 SELECT DISTINCT a.rlyr||'-'|| a.path_id as rid , a.rlyr,
    a.path_id,
    st_transform(st_buffer(st_transform(a.geom, 3161), 1000::double precision), 4326) AS geom
   FROM tur_shpath_v3 a;

ALTER TABLE public.tur_shpath_buffers_1km_v1
    OWNER TO postgres;
	
	select distinct rid from tur_shpath_buffers_1km_v1;
	