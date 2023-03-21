package com.polus.fibicomp.notification.render;

import java.util.Map;

public interface EmailRenderService {
	
	/**
	 * This method is used to build a map with placeholder text and value to replace the text
	 * @param moduleItemKey
	 * @return a map with placeholder and its value
	 */
	public Map<String, String> getPlaceHolderData(String moduleItemKey);

	/**
	 * This method is used to return the corresponding module code of the render service
	 * @return module code
	 */
	public String getModuleType();

	/**
	 * This method is used to return the corresponding submodule code of the render service
	 * @return submodule code
	 */
	public String getSubModuleCode();

}
