CREATE TABLE "User" (
    id serial PRIMARY KEY,
    username varchar (128) NOT NULL,
    password bytea NOT NULL,
    email varchar (128) NOT NULL
);


CREATE TABLE "Feed" (
    id serial PRIMARY KEY,
    name varchar (128) NOT NULL,
    url text NOT NULL,
    updateEvery interval NOT NULL
);


CREATE TABLE "Subscription" (
    feedId serial REFERENCES "Feed"(id) ON DELETE CASCADE,
    userId serial REFERENCES "User"(id) ON DELETE CASCADE,
    PRIMARY KEY(feedId, userId)
);


CREATE TABLE "FeedItem" (
   id serial PRIMARY KEY,
   feedId serial REFERENCES "Feed"(id) ON DELETE CASCADE,
   title text NOT NULL,
   url text NOT NULL,
   date timestamp with time zone NOT NULL,
   commentUrl text
);


CREATE TABLE "UnreadItem" (
    feedItemId serial REFERENCES "FeedItem"(id) ON DELETE CASCADE,
    userId serial REFERENCES "User"(id) ON DELETE CASCADE,
    PRIMARY KEY (feedItemId, userId)
);
