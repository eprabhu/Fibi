package com.polus.fibicomp.notification.render;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class EmailRenderFactory {

	/** The EmailRenderServices. */
	@Autowired
	private List<EmailRenderService> renderServices;

	/** The render service cache. */
	private final Map<String, ArrayList<Map<String,EmailRenderService>>> renderServiceCache = new HashMap<>();

	/**
	 * Inits the my service cache.
	 */
	@PostConstruct
	public void initMyServiceCache() {
		for (EmailRenderService renderService : renderServices) {
			Map<String, EmailRenderService> innerRenderServiceCache = new HashMap<>();
			ArrayList<Map<String, EmailRenderService>> innerRenderServiceList = null;
			innerRenderServiceCache.put(renderService.getSubModuleCode(), renderService);
			if(renderServiceCache.get(renderService.getModuleType()) == null) {
				innerRenderServiceList = new ArrayList<Map<String, EmailRenderService>>();
			} else {
				innerRenderServiceList = renderServiceCache.get(renderService.getModuleType());
			}
			innerRenderServiceList.add(innerRenderServiceCache);
			renderServiceCache.put(renderService.getModuleType(), innerRenderServiceList);
		}
	}

	/**
	 * Factory method to get the render service instance depending upon module code.
	 *
	 * @param moduleCode the code of module
	 * @return EmailRenderService
	 * @throws PlatformException the platform exception
	 */
	public EmailRenderService getRenderService(String moduleCode, String subModuleCode) throws Exception {
		EmailRenderService renderService = null;
		ArrayList<Map<String,EmailRenderService>> renderServiceList = renderServiceCache.get(moduleCode);
		if(renderServiceList != null && !renderServiceList.isEmpty()) {
			for(Map<String, EmailRenderService> innerRenderService : renderServiceList) {
				if(innerRenderService.get(subModuleCode) != null) {
					renderService = innerRenderService.get(subModuleCode);
					break;
				}
			}
		}
		if (null == renderService) {
			throw new IllegalArgumentException("No render service found for this module code.");
		}
		return renderService;
	}

}
