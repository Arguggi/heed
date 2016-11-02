CREATE TABLE "User" (
    id serial PRIMARY KEY,
    username varchar (128) NOT NULL,
    password bytea NOT NULL,
    email varchar (128) NOT NULL
);

CREATE TABLE "FeedInfo" (
    id serial PRIMARY KEY,
    name varchar (128) NOT NULL,
    url text NOT NULL,
    updateEvery integer NOT NULL,
    CONSTRAINT in_future CHECK (updateEvery > 0)
);

CREATE TABLE "Subscription" (
    feedInfoId serial REFERENCES "FeedInfo"(id) ON DELETE CASCADE NOT NULL,
    userId serial REFERENCES "User"(id) ON DELETE CASCADE NOT NULL,
    PRIMARY KEY(feedId, userId)
);

CREATE TABLE "FeedItem" (
   id serial PRIMARY KEY,
   feedInfoId serial REFERENCES "FeedInfo"(id) ON DELETE CASCADE NOT NULL,
   title text NOT NULL,
   url text NOT NULL,
   pubDate timestamp with time zone NOT NULL,
   commentUrl text
);

CREATE TABLE "UnreadItem" (
    feedItemId serial REFERENCES "FeedItem"(id) ON DELETE CASCADE NOT NULL,
    userId serial REFERENCES "User"(id) ON DELETE CASCADE NOT NULL,
    PRIMARY KEY (feedItemId, userId)
);
