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

function podcasts(state=initialPodcasts, action) {
    return state
}

export default podcasts;
