package uk.ac.cam.ws423.prejava.ex1;

public class PackedLong {
	public static boolean get(long packed, int position) {
		long check = (packed & (1 << position)) >> position;

		return (check == 1);
	}

	public static long set(long packed, int position, boolean value) {
        int v = 1;
		if (value) v = 0;

		return packed ^ (v << position);
	}
}
