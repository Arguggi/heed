CREATE TABLE user_feed_info_pref (
    user_id serial REFERENCES heed_user(id) ON DELETE CASCADE NOT NULL,
    feed_info_id serial REFERENCES feed_info(id) ON DELETE CASCADE NOT NULL,
    feed_info_name VARCHAR (128) NOT NULL
);
