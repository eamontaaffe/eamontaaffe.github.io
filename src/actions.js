import GitHub from 'github-api';

// about: {
//     isFetching: false,
//     content,
// }

export const REQUEST_ABOUT = "REQUEST_ABOUT";
export const RECEIVE_ABOUT = "RECEIVE_ABOUT";

export function requestAbout() {
    return {
	type: REQUEST_ABOUT,
    }
}

export function receiveAbout(content) {
    return {
	type: RECEIVE_ABOUT,
	content,
    }
}

const GITHUB_USER = "eamontaaffe";
const GITHUB_REPO = "content";
const GITHUB_REF = "master"
const ABOUT_PATH = "about.md"


/**
* TODO: I need to wrap this in a fetchIfRequired type function
* that prevents any re-fetching of data
*/  
export function fetchAbout() {
    return (dispatch) => {
	dispatch(requestAbout());
	const gh = new GitHub();
	const repo = gh.getRepo(GITHUB_USER,GITHUB_REPO);

	return repo.getContents(GITHUB_REF, ABOUT_PATH).
	    then(response => response.data).
	    then(data => {
		const content = atob(data.content)
		dispatch(receiveAbout(content))
	    }
		)
    }
}
