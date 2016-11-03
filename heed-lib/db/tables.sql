CREATE TABLE heed_user (
    id serial PRIMARY KEY,
    username varchar (128) NOT NULL,
    password bytea NOT NULL,
    email varchar (128) NOT NULL
);

CREATE TABLE feed_info (
    id serial PRIMARY KEY,
    name varchar (128) NOT NULL,
    url text NOT NULL,
    update_every integer NOT NULL,
    last_updated timestamp with time zone NOT NULL,
    CONSTRAINT in_future CHECK (update_every > 0)
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
