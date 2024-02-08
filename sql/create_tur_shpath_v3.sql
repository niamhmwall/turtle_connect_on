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
	