import GitHub from 'github-api';

const GITHUB_USER = "eamontaaffe";
const GITHUB_REPO = "content";
const GITHUB_REF = "master";

export const ABOUT_PATH = "about.md";
export const POSTS_PATH = "blog/";
export const BOOKS_PATH = "books.json";
export const PODCASTS_PATH = "podcasts.json";

const gh = new GitHub()
const repo = gh.getRepo(GITHUB_USER, GITHUB_REPO);

export const aboutPromise = repo.getContents(GITHUB_REF, ABOUT_PATH);
export const postsPromise = repo.getContents(GITHUB_REF, POSTS_PATH);
export const booksPromise = repo.getContents(GITHUB_REF, BOOKS_PATH);
export const podcastsPromise = repo.getContents(GITHUB_REF, PODCASTS_PATH);

export function contentPromise(path) {
    return repo.getContents(GITHUB_REF, path);
}

