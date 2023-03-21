package com.polus.fibicomp.sectionwiseedit.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.sectionwiseedit.pojo.SectionType;

@Transactional
@Service
public interface SectionWiseEditDao {

	/**
	 * This method is used to getAwardWorkFlowStatusByCode.
	 * @param statusCode - statusCode
	 * @return awardWorkflowStatus object
	 */
	public List<String> getSectionTypeCodeBasedOnTypeCode(String serviceRequestTypeCode);

	/**
	 * This method is used to checkSectionInModule.
	 * @param moduleCode - ModuleCode
	 * @param sectionCode - sectionCode
	 * @return boolean value
	 */
	public boolean checkSectionInModule(Integer moduleCode, String sectionCode);

	/**
	 * This method is used to checkSectionInModule.
	 * @param sectionCode - sectionCode
	 * @return object of SectionType
	 */
	public SectionType getSectionTypebySectionTypeId(String sectionCode);

	/**
	 * This method is used to saveorUpdateModuleVariableSection.
	 * @param moduleVariableSection - object of ModuleVariableSection
	 * @return object of ModuleVariableSection
	 */
	public ModuleVariableSection saveorUpdateModuleVariableSection(ModuleVariableSection moduleVariableSection);

	/**
	 * This method is used to getEditableSections.
	 * @param moduleItemKey - moduleItemKey
	 * @param moduleCode - moduleCode
	 * @return object of SectionType
	 */
	public List<ModuleVariableSection> getEditableSections(String moduleItemKey, String subModuleItemKey, Integer moduleCode, String personId, Integer submoduleCode);

	/**
	* This method is used to getTaskEditableSections.
	* @param moduleItemKey - moduleItemKey.
	* @param subModuleItemKey - subModuleItemKey.
	* @param moduleCode - moduleCode.
	* @param variableType- variableType.
	* @return object of SectionType.
	*/
	public List<ModuleVariableSection> getTaskEditableSections(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode,  String variableType, String personId);

	/**
	 * This method is used to getEditableSectionsDetails.
	 * @param moduleItemKey - moduleItemKey
	 * @param submoduleItemKey - subModuleItemKey
	 * @param moduleCode - moduleCode
	 * @param subModuleCode - subModuleCode
	 * @param personId - personId
	 * @param variableType - variableType
	 * @return List of Module Variable Sections
	 */
	public List<ModuleVariableSection> getModuleVariableSections(String moduleItemKey, String suModuleItemKey, Integer moduleCode, Integer subModuleCode, String personId, String variableType, Integer typeCode);

	/**
	 * This method is used to deleteModuleVariableSection.
	 * @param moduleVariableSection - moduleVariableSection
	 */
	public void deleteModuleVariableSection(ModuleVariableSection moduleVariableSection);

	/**
	 * This method is used to get know if we can change the budget status to submitted and create new budget in variation
	 * @param string
	 * @param submoduleItemKey
	 * @param awardModuleCode
	 * @param awardSubmoduleCode
	 * @return boolean
	 */
	public boolean isChangeBudgetStatus(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode, String sectionTypeCode);

	/**
	 * @param moduleItemKey
	 * @param submoduleItemKey
	 * @param awardModuleCode
	 * @param awardSubmoduleCode
	 * @param sectionTypeCode
	 * @return boolean
	 */
	public boolean isSectionEditableForActiveDocument(List<String> moduleItemKey, String submoduleItemKey, Integer awardModuleCode,
			Integer awardSubmoduleCode, String sectionTypeCode);

	/**
	 * @param awardNumber
	 * @return List of String values
	 */
	public List<String> getAwardEditingSectionsBasedOnParams(String awardNumber);

	/**
	 * This method is used to getEditableSections.
	 * @param moduleItemKey - moduleItemKey
	 * @param moduleCode - moduleCode
	 * @return string of SectionCodes
	 */
	public List<String> getEditableSectionCodes(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode);

	/**
	 * This method is used to getEditableSections based on moduleItemKeys.
	 * @param moduleItemKey - moduleItemKey
	 * @param moduleCode - moduleCode
	 * @return string of SectionCodes
	 */
	public List<ModuleVariableSection> getEditableSectionsByModuleItemKeys(List<String> moduleItemKeys, String submoduleItemKey, Integer awardModuleCode, Integer awardSubmoduleCode);

}
