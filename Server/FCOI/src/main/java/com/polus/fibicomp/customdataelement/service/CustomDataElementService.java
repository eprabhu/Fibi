package com.polus.fibicomp.customdataelement.service;

import com.polus.fibicomp.common.dto.ResponseData;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.customdataelement.vo.CustomDataElementVO;

import java.util.List;
import java.util.Map;

@Transactional
@Service
public interface CustomDataElementService {

	/**
	 * This method is used to create, update the CustomDataElements
	 * @param vo
	 * @return object of CustomelementVO as string
	 */
	public String configureCustomElement(CustomDataElementVO  vo);

	/**
	 * This method is used to fetch all the CustomDataElements
	 * @param vo
	 * @return object of CustomelementVO as string
	 */
	public String fetchAllCustomElement(CustomDataElementVO vo);

	/**
	 * This method is used to fetch the details of CustomDataElement by id
	 * @param vo
	 * @return object of CustomelementVO as string
	 */
	public String fetchCustomElementById(CustomDataElementVO vo);

	/**
	 * This method is used to delete the CustomDataElement by id
	 * @param vo
	 * @return object of CustomelementVO as string
	 */
	public String activeDeactivateCustomElementById(CustomDataElementVO vo);

	/**
	 * This method is used to get all the data types
	 * @return data types
	 */
	public String getCustomElementDataTypes();

	/**
	 * This method is used to save the answers of custom elements
	 * @param vo
	 * @return saved response
	 */
	public String saveCustomResponse(CustomDataElementVO vo);

	/**
	 * This method is used to get all the applicable custom elements based on module and module item key
	 * @param vo
	 * @return applicable custom elements
	 */
	public String getApplicableCustomElement(CustomDataElementVO vo);

	/**
	 * This method is used to get all module list
	 * @return
	 */
	public String getApplicableModules();

	/**
	 * This method is used to know if a module have the custom element
	 * @param moduleCode
	 * @return true if custom element applied else false
	 */
	public boolean isOtherInformationPresent(Integer moduleCode);

	/**
	 * This method is used get the system lookups
	 * @param vo
	 * @return lookup based on custom types
	 */
	public String getSystemLookupByCustomType(CustomDataElementVO vo);

	/**
	 * This method is used to copy common custom element data from one module to another.
	 * @param subModuleItemKey - Id of the module to which the elements needs to copy.
	 * @param subModuleCode - module code of the module to which the elements needs to copy.
	 * @param baseModuleItemKey - Id of the module from which the elements needs to copy.
	 * @param baseModuleCode - module code of the module from which the elements needs to copy.
	 * @param updateUser - Update username.
	 */
	public void copyCustomData(String subModuleItemKey, Integer subModuleCode, String baseModuleItemKey, Integer baseModuleCode, String updateUser);

	/**
	 * @param orginalModuleCode
	 * @param copiedModuleCode
	 * @param moduleItemKey
	 */
	public void copyCustomDataBasedOnModule (Integer orginalModuleCode, Integer copiedModuleCode, Integer moduleItemKey, Integer subModuleCode, String subModuleItemKey);

	/**
	 * Get Custom Elements by Module code
     *
	 * @param moduleCode Module code
	 * @return list of custom elements
	 */
	ResponseEntity<ResponseData> getCustomElements(Integer moduleCode);

	/**
	 * This service method is used to update the custom data element's order number
	 *
	 * @param cusEleOrderList
	 * @param  moduleCode
	 * @return status of the process
	 */
    ResponseEntity<ResponseData> saveCustomElementOrder(List<Map<String, Integer>> cusEleOrderList, Integer moduleCode);

	/**
	 * This service method is used to update custom element is required? or not?
	 *
	 * @param elementId custom data element primary key
	 * @param moduleCode custom data element usage module code
	 * @return status of the process
	 */
    ResponseEntity<ResponseData> updateCustomElementRequired(Integer elementId, Integer moduleCode);

	/**
	 * This service method is used to get custom element used modules
	 *
	 * @return list of usage modules
	 */
	ResponseEntity<ResponseData> getCusElementModules();
}
