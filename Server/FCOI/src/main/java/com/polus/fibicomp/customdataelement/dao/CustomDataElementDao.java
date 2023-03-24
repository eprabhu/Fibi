package com.polus.fibicomp.customdataelement.dao;

import java.util.List;

import com.polus.fibicomp.pojo.Module;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.customdataelement.pojo.CustomData;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementDataType;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementOption;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementUsage;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElements;
import com.polus.fibicomp.customdataelement.pojo.LookupWindow;

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
	 * This method is used to delete the custom element usage
	 * @param customDataElementUsage
	 */
	public void deleteCutsomElementUsage(CustomDataElementUsage customDataElementUsage);

	/**
	 * This method is used to delete the custom data element 
	 * @param customElementId
	 * @return message for deletion
	 */
	public String activeDeactivateCustomElementById(Integer customElementId);

	/**
	 * This method used to fetch all data types for custom elements
	 * @return list of custom elements
	 */
	public List<CustomDataElementDataType> getCustomElementDataTypes();

	/**
	 * This method is used to get the next version number of custom data element id
	 * @param columnId
	 * @return new version number
	 */
	public Integer getNextVersionNumber(Integer columnId);

	/**
	 * This method is used to update the is latest flag of old version to 'N'
	 * @param columnId
	 * @param customElememntId
	 */
	public void updateLatestFlag(Integer columnId, Integer customElememntId);

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
	 * This method is used to get all applicable custom elements based on modules
	 * @param moduleCode
	 * @return usage with all custom data elements
	 */
	public List<CustomDataElementUsage> getApplicableCustomElement(Integer moduleCode);
	
	/**
	 * This method used to get next column id
	 * @return new column id
	 */
	public Integer getNextColumnId();

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
	public List<String> getCustomDataOptions(Integer customElementId);
	
	/**
	 * This method is used get all responses for custom data
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param customElementId
	 * @return list of answers for custom data
	 */
	public List<CustomData> getCustomDataAnswers(Integer moduleCode,String moduleItemKey, Integer customElementId);

	/**
	 * This method is used to delete responses of custom options
	 * @param customDataId
	 */
	public void deleteOptionResponse(Integer customDataId);

	/**
	 * This method is used to get module item status based on module code and module item key
	 * @param moduleCode
	 * @param moduleItemKey
	 * @return status
	 */
	public Integer getModuleItemStatus(Integer moduleCode, String moduleItemKey);

	/**
	 * This method is used to get version of answered custom data  
	 * @param columnId
	 * @param moduleCode
	 * @param moduleItemKey
	 * @return version number
	 */
	public Integer getAnswerVersion(Integer columnId, Integer moduleCode, String moduleItemKey);

	/**
	 * This method is used to get the answered custom element id
	 * @param columnId
	 * @param moduleItemKey
	 * @param answerVersion
	 * @param moduleCode 
	 * @return custom element id
	 */
	public Integer getAnsweredCustomElementId(Integer columnId, String moduleItemKey, Integer answerVersion, Integer moduleCode);

	/**
	 * This method is used get all responses for custom data
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param subModuleItemKey 
	 * @param subModuleCode 
	 * @return list of answers for custom data
	 */
	public List<CustomData> fetchCustomDataByParams(Integer moduleCode, Integer moduleItemKey, Integer subModuleCode, String subModuleItemKey);

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
	 * This method is used get all responses for custom data.
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param customElementId
	 * @return
	 */
	public List<CustomData> getAllCustomDataAnswers(Integer moduleCode,String moduleItemKey, Integer customElementId);

	/**
	 * This method is used get common custom element usages.
	 * @param subModuleCode
	 * @param baseModuleCode
	 * @return
	 */
	public List<CustomDataElementUsage> getCommonCustomElements(Integer subModuleCode, Integer baseModuleCode);

	/**
	 * This method is used to get custom data elements based on param
	 * @param customElementName
	 * @return customDataElements
	 */
	public CustomDataElements fetchCustomDataElementDetail(String customElementName);

	/**
	 * This method is used to get custom data values based on params
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param customDataElementsId
	 * @return customData
	 */
	public CustomData getCustomDataValue(Integer moduleCode, String moduleItemKey, Integer customDataElementsId);

	/**
	 * Get Custom data elements by module code
	 *
	 * @param moduleCode module code
	 * @return list of custom data elements objects
	 */
    List<CustomDataElements> getCustomElementByModuleCode(Integer moduleCode);

	/**
	 * This DAO method used to update order number of a custom data element
	 *
	 * @param customElementId
	 * @param moduleCode
	 * @param orderNumber
	 */
    void saveCustomElementOrderNumber(Integer customElementId, Integer orderNumber, Integer moduleCode);

	/**
	 * This DAO method is used to update custom element is required? or not?
	 *
	 * @param elementId custom data element primary key
	 * @param moduleCode custom data element usage module code
	 */
    void updateCustomElementRequired(Integer elementId, Integer moduleCode);

	/**
	 * This DAO method is used to find the largest order number in a module
	 *
	 * @param moduleCode
	 * @return the largest order number in a module
	 */
	int findLargestCusElementOrderNumber(Integer moduleCode);

	/**
	 * This DAO method is used to get all distinct usage modules
	 *
	 * @return list of modules
	 */
	List<Module> getCusElementModules();
}
