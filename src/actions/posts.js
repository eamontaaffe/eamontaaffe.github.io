export const REQUEST_POSTS = "REQUEST_POSTS";
export const RECEIVE_POST = "RECEIVE_POST";

import { postsPromise, contentPromise } from "./github"

export function requestPosts() {
    return {
	type: REQUEST_POSTS,
    }
}

export function receivePost(title, date, content, id) {
    return {
	type: RECEIVE_POST,
	title,
	date,
	content,
        id,
    }
}

class Post {
    constructor(data) {
        this.data = data;
    }

    getFileName() {
        return this.data.name.split("_").join(" ");
    }

    getFileType() {
        const parts = this.getParts()
        return parts[parts.length -1]
    }

    getParts() {
        // 2017_01_14.Elixir_GenServer_breakdown.md
        return this.getFileName().split(".")
    }

    getTitle() {
        return this.getParts()[1]
    }

    getDate() {
        return new Date(this.getParts()[0].split("_").join("-"))
    }

    getContent() {
        return atob(this.data.content)
    }

    getId() {
        const parts = this.data.name.split(".")
        return parts.slice(0,2).join("_")
    }
}

function getPost(dispatch, data) {
    const post = new Post(data)
    dispatch(receivePost(
	post.getTitle(),
	post.getDate(),
	post.getContent(),
        post.getId(),
    ))
}

function getFileType(file) {
    const parts = file.name.split(".")
    return parts[parts.length-1]
}

function getAllPosts(dispatch, data) {
    data.reverse().forEach((file) => {
        if(getFileType(file) === 'md') {
	    contentPromise(file.path)
	        .then(response => response.data)
	        .then(data => getPost(dispatch, data))
        }
    })
}

export function fetchPosts() {
    return (dispatch) => {
	dispatch(requestPosts());

	return postsPromise()
	    .then(response => response.data)
	    .then(
		data => getAllPosts(dispatch, data)
            )
    }
}
