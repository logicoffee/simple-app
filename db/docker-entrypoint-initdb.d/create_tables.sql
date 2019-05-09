CREATE TABLE todo (
    id SERIAL PRIMARY KEY,
    task text NOT NULL,
    deadline date NOT NULL
);
