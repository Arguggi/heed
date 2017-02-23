CREATE TYPE itemdates AS ENUM ('missing', 'present');

CREATE TABLE heed_user (
    id serial PRIMARY KEY,
    username varchar (128) NOT NULL,
    password text NOT NULL,
    email varchar (128) NOT NULL
);

CREATE TABLE feed_info (
    id serial PRIMARY KEY,
    name varchar (128) NOT NULL,
    url text NOT NULL,
    update_every integer NOT NULL,
    last_updated timestamp with time zone NOT NULL,
    has_item_date itemdates NOT NULL,
    number_items int NOT NULL,
    CONSTRAINT in_future CHECK (update_every > 0),
    CONSTRAINT positive_number_items CHECK (number_items > 0)
);

CREATE TABLE subscription (
    feed_info_id serial REFERENCES feed_info(id) ON DELETE CASCADE NOT NULL,
    user_id serial REFERENCES heed_user(id) ON DELETE CASCADE NOT NULL,
    PRIMARY KEY(feed_info_id, user_id)
);

CREATE TABLE feed_item (
    id serial PRIMARY KEY,
    feed_info_id serial REFERENCES feed_info(id) ON DELETE CASCADE NOT NULL,
    title text NOT NULL,
    url text NOT NULL,
    pub_date timestamp with time zone NOT NULL,
    comment_url text
);

CREATE TABLE unread_item (
    feed_item_id serial REFERENCES feed_item(id) ON DELETE CASCADE NOT NULL,
    user_id serial REFERENCES heed_user(id) ON DELETE CASCADE NOT NULL,
    PRIMARY KEY (feed_item_id, user_id)
);

CREATE TABLE auth_token (
    user_id serial REFERENCES heed_user(id) ON DELETE CASCADE NOT NULL,
    token text NOT NULL,
    PRIMARY KEY (user_id)
);


CREATE OR REPLACE FUNCTION add_id_token() RETURNS TRIGGER AS
$BODY$
BEGIN
    INSERT INTO
        auth_token(user_id,token)
        VALUES(new.id,'invalid');
    RETURN new;
END;
$BODY$
language plpgsql;

CREATE TRIGGER after_new_user
    AFTER INSERT ON heed_user
    FOR EACH ROW
        EXECUTE PROCEDURE add_id_token();
