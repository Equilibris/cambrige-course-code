package uk.ac.cam.cl.gfxintro.ws423.tick1;

import java.awt.image.BufferedImage;
import java.util.List;

public class Renderer {
	
	// The width and height of the image in pixels
	private int width, height;
	
	// Bias factor for reflected and shadow rays
	private final double EPSILON = 0.0001;

	// The number of times a ray can bounce for reflection
	private int bounces;

	private final int SHADOW_RAY_COUNT = 10;
	private final double LIGHT_SIZE = 0.2;



	// Background colour of the image
	private ColorRGB backgroundColor = new ColorRGB(0.001);

	public Renderer(int width, int height, int bounces) {
		this.width = width;
		this.height = height;
		this.bounces = bounces;
	}

	/*
	 * Trace the ray through the supplied scene, returning the colour to be rendered.
	 * The bouncesLeft parameter is for rendering reflective surfaces.
	 */
	protected ColorRGB trace(Scene scene, Ray ray, int bouncesLeft) {

		// Find closest intersection of ray in the scene
		RaycastHit closestHit = scene.findClosestIntersection(ray);

        // If no object has been hit, return a background colour
        SceneObject object = closestHit.getObjectHit();
        if (object == null) return backgroundColor;
        
        // Otherwise calculate colour at intersection and return
        // Get properties of surface at intersection - location, surface normal
        Vector3 P = closestHit.getLocation();
        Vector3 N = closestHit.getNormal();
        Vector3 O = ray.getOrigin();

     	// Illuminate the surface

     	ColorRGB direct = this.illuminate(scene, object, P, N, O);

		double reflectivity = object.getReflectivity();
		if (bouncesLeft == 0 || reflectivity == 0) return direct;

		Vector3 refl = O.subtract(P).normalised().reflectIn(N);
		Ray cast = new Ray(P.add(refl.scale(EPSILON)), refl);
		ColorRGB reflect = trace(scene, cast, bouncesLeft - 1);

		return direct
				.scale(1. - reflectivity)
				.add(
						reflect.scale(reflectivity)
				);
	}



	/*
	 * Illuminate a surface on and object in the scene at a given position P and surface normal N,
	 * relative to ray originating at O
	 */
	private ColorRGB illuminate(Scene scene, SceneObject object, Vector3 P, Vector3 N, Vector3 O) {
	   
		//ColorRGB colourToReturn = new ColorRGB(1./(P.magnitude() * 20));
		ColorRGB colourToReturn = new ColorRGB(0);

		ColorRGB I_a = scene.getAmbientLighting(); // Ambient illumination intensity

		ColorRGB C_diff = object.getColour(); // Diffuse colour defined by the object
		
		// Get Phong reflection model coefficients
		double k_d = object.getPhong_kD();
		double k_s = object.getPhong_kS();
		double alpha = object.getPhong_alpha();

		// TODO: Add ambient light term to start with

		colourToReturn =  C_diff.scale(I_a);

		// Loop over each point light source
		List<PointLight> pointLights = scene.getPointLights();
		for (int i = 0; i < pointLights.size(); i++) {
			PointLight light = pointLights.get(i); // Select point light
			
			// Calculate point light constants
			Vector3 direction_vec = light.getPosition().subtract(P);
			double distanceToLight = direction_vec.magnitude();
			ColorRGB C_spec = light.getColour();
			ColorRGB I = light.getIlluminationAt(distanceToLight);

			// TODO: Calculate L, V, R
			// TODO: Calculate ColorRGB diffuse and ColorRGB specular terms
			// TODO: Add these terms to colourToReturn

			Vector3 l = direction_vec.normalised();

			double shadow_prop = 1.;

			for (int j = 0; j < SHADOW_RAY_COUNT; j++) {
				Ray shadowray = new Ray(P.add(N.scale(EPSILON)), l.add(Vector3.randomInsideUnitSphere().scale(LIGHT_SIZE)));

				double closestHit = scene.findClosestIntersection(shadowray).getDistance();
				if (closestHit < distanceToLight) shadow_prop -= 1./SHADOW_RAY_COUNT;
			}


			ColorRGB diffuse  = C_diff
					.scale(k_d * Math.max(0., N.dot(l)))
					.scale(I);

			Vector3 v = O.subtract(P).normalised();
			Vector3 r = l.reflectIn(N);
			ColorRGB specular = C_spec
					.scale(k_s)
					.scale(I)
					.scale(Math.pow(Math.max(0., r.dot(v)), alpha));

			colourToReturn = colourToReturn
					.add(diffuse.scale(shadow_prop))
					.add(specular.scale(shadow_prop))
			;
		}
		return colourToReturn;
	}

	// Render image from scene, with camera at origin
	public BufferedImage render(Scene scene) {
		
		// Set up image
		BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		
		// Set up camera
		Camera camera = new Camera(width, height);

		// Loop over all pixels
		for (int y = 0; y < height; ++y) {
			for (int x = 0; x < width; ++x) {
				Ray ray = camera.castRay(x, y); // Cast ray through pixel
				ColorRGB linearRGB = trace(scene, ray, bounces); // Trace path of cast ray and determine colour
				ColorRGB gammaRGB = tonemap( linearRGB );
				image.setRGB(x, y, gammaRGB.toRGB()); // Set image colour to traced colour
			}
			// Display progress every 10 lines
            if( y % 10 == 9 | y==(height-1) )
			    System.out.println(String.format("%.2f", 100 * y / (float) (height - 1)) + "% completed");
		}
		return image;
	}


	// Combined tone mapping and display encoding
	public ColorRGB tonemap( ColorRGB linearRGB ) {
		double invGamma = 1./2.2;
		double a = 2;  // controls brightness
		double b = 1.3; // controls contrast

		// Sigmoidal tone mapping
		ColorRGB powRGB = linearRGB.power(b);
		ColorRGB displayRGB = powRGB.scale( powRGB.add(Math.pow(0.5/a,b)).inv() );

		// Display encoding - gamma
		ColorRGB gammaRGB = displayRGB.power( invGamma );

		return gammaRGB;
	}
}
