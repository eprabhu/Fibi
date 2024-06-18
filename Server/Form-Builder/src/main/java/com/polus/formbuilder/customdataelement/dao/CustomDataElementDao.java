package com.polus.formbuilder.customdataelement.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.appcorelib.pojo.LookupWindow;
import com.polus.formbuilder.customdataelement.pojo.CustomData;
import com.polus.formbuilder.customdataelement.pojo.CustomDataElementOption;
import com.polus.formbuilder.customdataelement.pojo.CustomDataElements;

@Transactional
@Service
public interface CustomDataElementDao {
	
	/**
	 * This method is used to save or update the CustomDataElements
	 * @param customDataElement
	 * @return updated or inserted CustomDataElement object
	 */
	public CustomDataElements saveOrUpdateCustomElement(CustomDataElements customDataElement);

	/**
	 * This method used to fetch all CustomDataElements
	 * @return list of custom data elements
	 */
	public List<CustomDataElements> fetchAllCustomElements();

	/**
	 * This method used to fetch CustomDataElements by id
	 * @param customElementId
	 * @return details of custom data elements
	 */
	public CustomDataElements fetchCustomElementById(Integer customElementId);

	/**
	 * This method is used to save or update the custom data element's options
	 * @param elementOption
	 * @return saved option
	 */
	public CustomDataElementOption saveOrUpdateElementOptions(CustomDataElementOption elementOption);

	/**
	 * This method is used to save the response for custom data element
	 * @param customResponse
	 * @return saved answer of custom data element
	 */
	public CustomData saveOrUpdateCustomResponse(CustomData customResponse);

	/**
	 * This method is used to get all options based on custom element id
	 * @param customElementId
	 * @return list of option based on custom element id
	 */
	public List<CustomDataElementOption> getCustomOptions(Integer customElementId);

	/**
	 * This method is used to delete custom element option
	 * @param option
	 */
	public void deleteCustomOption(CustomDataElementOption option);

	/**
	 * This method is used to get all options based on custom element id
	 * @param customElementId
	 * @return list of options
	 */
	public List<Object> getCustomDataOptions(Integer customElementId);
	
	/**
	 * This method is used get all responses for custom data
	 * @param moduleItemKey
	 * @param moduleCode
	 * @param moduleSubItemKey
	 * @param subModuleCode
	 * @param customElementId
	 * @return list of answers for custom data
	 */
	public List<CustomData> getCustomDataAnswers(String moduleItemKey, Integer moduleCode, String moduleSubItemKey,
			Integer subModuleCode, Integer customElementId);

	/**
	 * This method is used to delete responses of custom options
	 * @param customDataId
	 */
	public void deleteOptionResponse(Integer customDataId);

	/**
	 * This method is used to get the System lookup by data type code
	 * @param dataTypeCode
	 * @return list of lookups available in system
	 */
	public List<LookupWindow> getSystemLookupByCustomType(String dataTypeCode);

	/**
	 * This method is used to check whether custom element name is already exist
	 * @param customElementName
	 * @return boolean - true if exist else false
	 */
	public boolean isCustomElementNameExist(String customElementName);

	/**
	 * This method is used to fetch the lookup datas that are defined by the user
	 * 
	 * @return list of user defined lookups
	 */
	public List<String> getUserDeffinedLookup();

}
