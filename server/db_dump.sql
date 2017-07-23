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


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: items; Type: TABLE; Schema: public; Owner: shopshare
--

CREATE TABLE items (
    id integer NOT NULL,
    description text,
    completed boolean DEFAULT false NOT NULL,
    list_id integer
);


ALTER TABLE items OWNER TO shopshare;

--
-- Name: items_id_seq; Type: SEQUENCE; Schema: public; Owner: shopshare
--

CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE items_id_seq OWNER TO shopshare;

--
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shopshare
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- Name: lists; Type: TABLE; Schema: public; Owner: shopshare
--

CREATE TABLE lists (
    id integer NOT NULL,
    title character varying(100)
);


ALTER TABLE lists OWNER TO shopshare;

--
-- Name: lists_id_seq; Type: SEQUENCE; Schema: public; Owner: shopshare
--

CREATE SEQUENCE lists_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE lists_id_seq OWNER TO shopshare;

--
-- Name: lists_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shopshare
--

ALTER SEQUENCE lists_id_seq OWNED BY lists.id;


--
-- Name: items id; Type: DEFAULT; Schema: public; Owner: shopshare
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


--
-- Name: lists id; Type: DEFAULT; Schema: public; Owner: shopshare
--

ALTER TABLE ONLY lists ALTER COLUMN id SET DEFAULT nextval('lists_id_seq'::regclass);


--
-- Data for Name: items; Type: TABLE DATA; Schema: public; Owner: shopshare
--

COPY items (id, description, completed, list_id) FROM stdin;
1	Milk	f	1
\.


--
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: public; Owner: shopshare
--

SELECT pg_catalog.setval('items_id_seq', 1, true);


--
-- Data for Name: lists; Type: TABLE DATA; Schema: public; Owner: shopshare
--

COPY lists (id, title) FROM stdin;
1	Test shopping list
\.


--
-- Name: lists_id_seq; Type: SEQUENCE SET; Schema: public; Owner: shopshare
--

SELECT pg_catalog.setval('lists_id_seq', 1, true);


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

