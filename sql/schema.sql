-- Table: public.node

-- DROP TABLE public.node;

CREATE SCHEMA graphs
    AUTHORIZATION postgres;

-- Table: graphs.node

-- DROP TABLE graphs.node;

CREATE TABLE graphs.node
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY
        ( INCREMENT 1 START 1 MINVALUE 1 MAXVALUE 2147483647 CACHE 1 ),
    label text COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT node_pkey PRIMARY KEY (id)
)

TABLESPACE pg_default;

ALTER TABLE graphs.node
    OWNER to postgres;

-- Table: graphs.edge

-- DROP TABLE graphs.edge;

CREATE TABLE graphs.edge
(
    id_from integer NOT NULL,
    id_to integer NOT NULL,
    CONSTRAINT edge_pkey PRIMARY KEY (id_from, id_to),
    CONSTRAINT node_from FOREIGN KEY (id_from)
        REFERENCES graphs.node (id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE CASCADE,
    CONSTRAINT node_to FOREIGN KEY (id_to)
        REFERENCES graphs.node (id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE CASCADE,
    CONSTRAINT loop CHECK (id_from <> id_to)
)

TABLESPACE pg_default;

ALTER TABLE graphs.edge
    OWNER to postgres;
