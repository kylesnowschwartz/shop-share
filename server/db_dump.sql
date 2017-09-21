--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.3
-- Dumped by pg_dump version 9.6.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: clients; Type: TABLE; Schema: public; Owner: shopshare
--

CREATE TABLE clients (
    id integer NOT NULL
);


ALTER TABLE clients OWNER TO shopshare;

--
-- Name: clients_id_seq; Type: SEQUENCE; Schema: public; Owner: shopshare
--

CREATE SEQUENCE clients_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE clients_id_seq OWNER TO shopshare;

--
-- Name: clients_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shopshare
--

ALTER SEQUENCE clients_id_seq OWNED BY clients.id;


--
-- Name: items; Type: TABLE; Schema: public; Owner: shopshare
--

CREATE TABLE items (
    id uuid DEFAULT uuid_generate_v4() NOT NULL,
    text text DEFAULT ''::text NOT NULL,
    completed boolean DEFAULT false NOT NULL,
    list_id uuid NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE items OWNER TO shopshare;

--
-- Name: lists; Type: TABLE; Schema: public; Owner: shopshare
--

CREATE TABLE lists (
    id uuid DEFAULT uuid_generate_v4() NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE lists OWNER TO shopshare;

--
-- Name: clients id; Type: DEFAULT; Schema: public; Owner: shopshare
--

ALTER TABLE ONLY clients ALTER COLUMN id SET DEFAULT nextval('clients_id_seq'::regclass);


--
-- Data for Name: clients; Type: TABLE DATA; Schema: public; Owner: shopshare
--

COPY clients (id) FROM stdin;
1
\.


--
-- Name: clients_id_seq; Type: SEQUENCE SET; Schema: public; Owner: shopshare
--

SELECT pg_catalog.setval('clients_id_seq', 1, false);


--
-- Data for Name: items; Type: TABLE DATA; Schema: public; Owner: shopshare
--

COPY items (id, text, completed, list_id, created_at, updated_at) FROM stdin;
f1bd4a84-d3e6-44a1-a064-17efbd02078d	adfasdf	f	becfac80-8dc9-4697-bf90-a95ff7a61326	2017-09-21 18:24:46.879647+12	2017-09-21 18:24:46.879647+12
5f1fe9aa-ac22-4cc2-956f-cd7b327eecef		f	9050b09c-fee9-42aa-aef6-9248924f1f32	2017-09-21 23:55:42.116461+12	2017-09-21 23:55:42.116461+12
d77bd66c-9ea2-4df3-8198-ce812a45c664	fffff	t	9050b09c-fee9-42aa-aef6-9248924f1f32	2017-09-21 23:55:26.537438+12	2017-09-21 23:55:26.537438+12
6e7d48f0-b260-4aac-a815-ad203eed7da4		t	9050b09c-fee9-42aa-aef6-9248924f1f32	2017-09-21 23:55:42.587095+12	2017-09-21 23:55:42.587095+12
\.


--
-- Data for Name: lists; Type: TABLE DATA; Schema: public; Owner: shopshare
--

COPY lists (id, title, created_at, updated_at) FROM stdin;
becfac80-8dc9-4697-bf90-a95ff7a61326		2017-09-21 18:22:46.307189+12	2017-09-21 18:22:46.307189+12
9050b09c-fee9-42aa-aef6-9248924f1f32		2017-09-21 23:55:23.689109+12	2017-09-21 23:55:23.689109+12
\.


--
-- Name: clients clients_pkey; Type: CONSTRAINT; Schema: public; Owner: shopshare
--

ALTER TABLE ONLY clients
    ADD CONSTRAINT clients_pkey PRIMARY KEY (id);


--
-- Name: items items_pkey; Type: CONSTRAINT; Schema: public; Owner: shopshare
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- Name: lists lists_pkey; Type: CONSTRAINT; Schema: public; Owner: shopshare
--

ALTER TABLE ONLY lists
    ADD CONSTRAINT lists_pkey PRIMARY KEY (id);


--
-- Name: items items_list_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shopshare
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_list_id_fkey FOREIGN KEY (list_id) REFERENCES lists(id);


--
-- PostgreSQL database dump complete
--

