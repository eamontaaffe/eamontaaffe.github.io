import { combineReducers } from 'redux';
import dir from './dir';
import about from './about';
import books from './books';
import podcasts from './podcasts';
import posts from './posts';

const rootReducer = combineReducers({
    dir,
    posts,
    about,
    books,
    podcasts,
})

export default rootReducer;
