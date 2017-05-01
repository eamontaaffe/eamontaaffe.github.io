const initialDir = [
    {
	id: 1,
	name: "/",
	alias: "home/",
	contents: [2,3,4,5],
    },
    {
	id: 2,
	name: "about/",
    },
    {
	id: 3,
	name: "blog/",
    },
    {
	id:4,
	name: "books/",
    },
    {
	id: 5,
	name: "podcasts/",
    },
]

const initialPosts = {
    isFetching: false,
    content: [],
}

const initialAbout = {
    isFetching: false,
    content: "",
}

const initialBooks = [
    {
	title: "The Lean Startup",
	author: "Eric Ries",
	link: "https://www.bookdepository.com/Lean-Startup-Eric-Ries/9780307887894"
    },
    {
	title: "Green Eggs and Ham",
	author: "Dr. Seuss",
	link: "https://www.bookdepository.com/Dr-Seuss---Green-Back-Book-Green-Eggs-and-Ham-Green-Back-Book-Dr-Seuss/9780007158461?ref=grid-view&qid=1493446644292&sr=1-2"
    },
    {
	title: "The Lean Startup",
	author: "Eric Ries",
	link: "https://www.bookdepository.com/Lean-Startup-Eric-Ries/9780307887894"
    },
    {
	title: "Green Eggs and Ham",
	author: "Dr. Seuss",
	link: "https://www.bookdepository.com/Dr-Seuss---Green-Back-Book-Green-Eggs-and-Ham-Green-Back-Book-Dr-Seuss/9780007158461?ref=grid-view&qid=1493446644292&sr=1-2"
    },
    {
	title: "The Lean Startup",
	author: "Eric Ries",
	link: "https://www.bookdepository.com/Lean-Startup-Eric-Ries/9780307887894"
    },
    {
	title: "Green Eggs and Ham",
	author: "Dr. Seuss",
	link: "https://www.bookdepository.com/Dr-Seuss---Green-Back-Book-Green-Eggs-and-Ham-Green-Back-Book-Dr-Seuss/9780007158461?ref=grid-view&qid=1493446644292&sr=1-2"
    },
]

const initialPodcasts = [
    {
	name: "#95 Silence in the Sky",
	producer: "Reply All",
	length: "0:45:32",
	link: "https://gimletmedia.com/episode/95-the-silence-in-the-sky/",
    },
    {
	name: "Funnky Hand Jive",
	producer: "Radiolab",
	length: "0:28:46",
	link: "http://www.radiolab.org/story/funky-hand-jive/",
    },
    {
	name: "The Honky Tonk Nun",
	producer: "Seriously",
	length: "1:15:02",
	link: "http://www.bbc.co.uk/programmes/p0512ywc",
    },
]

import {
    REQUEST_ABOUT,
    RECEIVE_ABOUT,
    REQUEST_POSTS,
    RECEIVE_POST,
} from './actions';
import { combineReducers } from 'redux';

function about(state = initialAbout, action) {
    switch (action.type) {
    case REQUEST_ABOUT:
	return {
	    ...state,
	    isFetching: true
	}
    case RECEIVE_ABOUT:
	return {
	    ...state,
	    isFetching: false,
	    content: action.content,
	}
    default:
	return state
    }
}

function podcasts(state=initialPodcasts, action) {
    return state
}

function books(state=initialBooks, action) {
    return state
}

function posts(state=initialPosts, action) {
    switch (action.type) {
    case REQUEST_POSTS:
	return {
	    isFetching: true,
	    content: [],
	};
    case RECEIVE_POST:
	const post = {
	    title: action.title,
	    date: action.date,
	    content: action.content,
	}
	return {
	    isFetching: false,
	    content: [
		...state.content,
		post,
	    ],
	};
    default:
	return state;
    }
}

function dir(state=initialDir, action) {
    return state
}

const rootReducer = combineReducers({
    dir,
    posts,
    about,
    books,
    podcasts,
})

export default rootReducer;
