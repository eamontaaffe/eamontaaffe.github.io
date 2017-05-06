import { booksPromise } from "./github";

export const REQUEST_BOOKS = "REQUEST_BOOKS";
export const RECEIVE_BOOKS = "RECEIVE_BOOKS";

function requestBooks() {
    return {
        type: REQUEST_BOOKS,
    }
}

function receiveAbout(content) {
    return {
        type: RECEIVE_BOOKS,
        content: content,
    }
}

export function fetchBooks() {
    return (dispatch) => {
        dispatch(requestBooks());

        return booksPromise()
            .then(response => response.data.content)
            .then(base64 => atob(base64))
            .then(data => JSON.parse(data))
            .then(json => json.books)
            .then(books => dispatch(receiveAbout(books)))
    }
}
