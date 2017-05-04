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

function dir(state=initialDir, action) {
    return state
}

export default dir;
