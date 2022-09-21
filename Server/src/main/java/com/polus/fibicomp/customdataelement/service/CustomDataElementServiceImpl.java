package com.polus.fibicomp.customdataelement.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.common.dto.ResponseData;
import com.polus.fibicomp.pojo.Module;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.dao.CustomDataElementDao;
import com.polus.fibicomp.customdataelement.pojo.CustomData;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementOption;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementUsage;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElements;
import com.polus.fibicomp.customdataelement.pojo.LookupWindow;
import com.polus.fibicomp.customdataelement.vo.CustomDataElementVO;
import com.polus.fibicomp.customdataelement.vo.CustomDataResponse;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.LookUp;

@Transactional
@Service(value = "customDataElementService")
public class CustomDataElementServiceImpl implements CustomDataElementService {

	@Autowired
	private CustomDataElementDao customDataElementDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardService awardService;

	private static final String CHECKBOX_ANSWER_VALUE = "true";

	protected static Logger logger = LogManager.getLogger(CustomDataElementServiceImpl.class.getName());

	@Override
	public String configureCustomElement(CustomDataElementVO vo) {
		CustomDataElements customDataElement = vo.getCustomDataElement();
		if (customDataElement.getAcType().equals("I")) {
			vo = saveOrUpdateCustomElements(vo);
		} else if (customDataElement.getAcType().equals("U")) {
			List<CustomDataElementUsage> customDataElementUsages = customDataElement.getCustomDataElementUsage();
			List<CustomDataElementUsage> insertCutsomElementUsage = new ArrayList<>();
			List<CustomDataElementUsage> updatedlist = new ArrayList<>(customDataElementUsages);
			Collections.copy(updatedlist, customDataElementUsages);
			for (CustomDataElementUsage customDataElementUsage : customDataElementUsages) {
				if (customDataElementUsage.getAcType().equals("D")) {
					customDataElementDao.deleteCutsomElementUsage(customDataElementUsage);
					updatedlist.remove(customDataElementUsage);
				}
				if (customDataElementUsage.getOrderNumber() == null && !customDataElementUsage.getAcType().equals("D")){
					customDataElementUsage.setOrderNumber(customDataElementDao.findLargestCusElementOrderNumber(customDataElementUsage.getModuleCode()));
				}
//			This code is intentionally commented till finalize the version criteria - by Arjun
//				if (Boolean.TRUE.equals(vo.getIsDataChange())) {
//					customDataElementUsage.setCustomElementUsageId(null);
//				} 
			}
			customDataElement.getCustomDataElementUsage().clear();
			customDataElement.getCustomDataElementUsage().addAll(updatedlist);
			if (insertCutsomElementUsage != null && !insertCutsomElementUsage.isEmpty()) {
				customDataElement.getCustomDataElementUsage().addAll(insertCutsomElementUsage);
			}
			if (vo.getDeleteOptions() != null && !vo.getDeleteOptions().isEmpty()) {
				deleteOptions(vo.getDeleteOptions());
			}
//			This code is intentionally commented till finalize the version criteria - by Arjun
//			if (Boolean.TRUE.equals(vo.getIsDataChange())) {
//				customDataElement.setCustomElementId(null);
//				customDataElement.setColumnVersionNumber(getNextVersionNumber(customDataElement.getColumnId()));
//			}
			customDataElement = customDataElementDao.saveOrUpdateCustomElement(customDataElement);
			updateLatestFlag(customDataElement.getColumnId(), customDataElement.getCustomElementId());
			vo.setCustomDataElement(customDataElement);
			if (customDataElement.getDataType().equals("4") || customDataElement.getDataType().equals("5")) {
				vo.setElementOptions(maintainOptions(vo));
			}
			vo.setResponseMessage("Custom data element updated successfully");
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchAllCustomElement(CustomDataElementVO vo) {
		vo.setCustomDataElements(customDataElementDao.fetchAllCustomElements());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchCustomElementById(CustomDataElementVO vo) {
		vo.setCustomDataElement(customDataElementDao.fetchCustomElementById(vo.getCustomDataElementId()));
		vo = getCustomOptions(vo);
		return commonDao.convertObjectToJSON(vo);

	}

	@Override
	public String activeDeactivateCustomElementById(CustomDataElementVO vo) {
		vo.setResponseMessage(customDataElementDao.activeDeactivateCustomElementById(vo.getCustomDataElementId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getCustomElementDataTypes() {
		CustomDataElementVO vo = new CustomDataElementVO();
		vo.setCustomDataTypes(customDataElementDao.getCustomElementDataTypes());
		return commonDao.convertObjectToJSON(vo);
	}

	public Integer getNextVersionNumber(Integer columnId) {
		return customDataElementDao.getNextVersionNumber(columnId);
	}

	public void updateLatestFlag(Integer columnId, Integer customElementId) {
		customDataElementDao.updateLatestFlag(columnId, customElementId);
	}

	public CustomDataElementVO saveOrUpdateCustomElements(CustomDataElementVO vo) {
		CustomDataElements customDataElement = vo.getCustomDataElement();
		if (customDataElementDao.isCustomElementNameExist(customDataElement.getCustomElementName())) {
			vo.setResponseMessage("Custom element name already exists");
			return vo;
		}
		if (customDataElement.getColumnId() == null) {
			customDataElement.setColumnId(customDataElementDao.getNextColumnId());
		}
		customDataElement.getCustomDataElementUsage().forEach(usage -> {
			usage.setOrderNumber(customDataElementDao.findLargestCusElementOrderNumber(usage.getModuleCode()));
		});
		customDataElement = customDataElementDao.saveOrUpdateCustomElement(customDataElement);
		customDataElement.setAcType("U");
		vo.setCustomDataElement(customDataElement);
		if ((customDataElement.getDataType().equals("4") || customDataElement.getDataType().equals("5")) && (vo.getElementOptions() != null && !vo.getElementOptions().isEmpty())) {
			for (CustomDataElementOption elementOption : vo.getElementOptions()) {
				elementOption.setCustomDataElementsId(customDataElement.getCustomElementId());
				if (elementOption.getOptionName() != null) {
					customDataElementDao.saveOrUpdateElementOptions(elementOption);
					elementOption.setAcType("U");
				}
			}
		}
		vo.setResponseMessage("Custom data element saved successfully");
		return vo;
	}

	@Override
	public String saveCustomResponse(CustomDataElementVO vo) {
		if (vo.getCustomElements() != null && !vo.getCustomElements().isEmpty()) {
			for (CustomDataResponse customElement : vo.getCustomElements()) {
				for (CustomData answer : customElement.getAnswers()) {
					if (canSaveTheCustomDataResponse(answer, customElement)) {
						CustomData customData = new CustomData();
						if (answer.getCustomDataId() != null) {
							customData.setCustomDataId(answer.getCustomDataId());
						}
						if (answer.getValue() == null && (customElement.getDataType().equals("1") || customElement.getDataType().equals("2"))) {
							customData.setValue(customElement.getDefaultValue());
						} else if (answer.getValue() != null) {
							customData.setValue(answer.getValue());
						}
						customData.setDescription(answer.getDescription());
						customData.setCustomDataElementsId(customElement.getCustomDataElementId());
						customData.setModuleItemCode(vo.getModuleCode());
						if (vo.getModuleItemKey() != null) {
							customData.setModuleItemKey(vo.getModuleItemKey().toString());
						}
						customData.setModuleSubItemCode(0);
						customData.setModuleSubItemKey("0");
						customData.setColumnId(customElement.getColumnId());
						customData.setVersionNumber(customElement.getVersionNumber());
						customData.setUpdateTimestamp(vo.getUpdateTimestamp());
						customData.setUpdateUser(vo.getUpdateUser());
						if (customElement.getDataType().equals("4") && customData.getCustomDataId() != null) {
							customDataElementDao.deleteOptionResponse(answer.getCustomDataId());
							customData.setCustomDataId(null);
						}
						customData = customDataElementDao.saveOrUpdateCustomResponse(customData);
						answer.setCustomDataId(customData.getCustomDataId());
					}
				}
			}
		}
		setDocumentUpdateUserAndTimestamp(vo.getModuleCode(), vo.getModuleItemKey(), vo.getUpdateUser());
		return commonDao.convertObjectToJSON(vo);
	}

	private boolean canSaveTheCustomDataResponse(CustomData answer, CustomDataResponse customElement) {
		return ((answer.getValue() != null && !answer.getValue().isEmpty())
				|| (customElement.getDefaultValue() != null && !customElement.getDefaultValue().isEmpty()
						&& (customElement.getDataType().equals("1") || customElement.getDataType().equals("2")))
				|| answer.getCustomDataId() != null);
	}

	@Override
	public String getApplicableCustomElement(CustomDataElementVO vo) {
		Integer moduleCode = vo.getModuleCode();
		String moduleItemKey = vo.getModuleItemKey().toString();
		List<CustomDataElementUsage> usages = customDataElementDao.getApplicableCustomElement(moduleCode);
		List<CustomDataResponse> customDataResponses = new ArrayList<>();
		for (CustomDataElementUsage usage : usages) {
			Integer answerVersion = null;
			 if (usage.getCustomDataElement().getIsLatestVesrion().equals("Y") && usage.getCustomDataElement().getIsActive().equals("N")) {
				answerVersion = customDataElementDao.getAnswerVersion(usage.getCustomDataElement().getColumnId(), moduleCode, moduleItemKey);
			}
			if ((usage.getCustomDataElement().getIsLatestVesrion().equals("Y") && usage.getCustomDataElement().getIsActive().equals("Y")) || answerVersion != null) {
				customDataResponses.add(setResponseObject(usage, moduleCode, moduleItemKey));
			}
		}
		vo.setCustomElements(customDataResponses);
// 		This function call is intentionally commented and using for multiple version scenario for custom data in fibi so don't remove this code ----by Arjun Kr
//		vo.setCustomDataElements(applicableCustomElemetsForMultipleVersion(usages, moduleCode, moduleItemKey));
		return commonDao.convertObjectToJSON(vo);
	}

	
	/**
	 * This Method is using for the multiple version scenario custom data in fibi so don't remove this code of block ----by Arjun Kr
	 */
	private List<CustomDataResponse> applicableCustomElemetsForMultipleVersion(List<CustomDataElementUsage> usages, Integer moduleCode, String moduleItemKey) {
		List<CustomDataResponse> customDataResponses = new ArrayList<>();
		Integer statusCode = customDataElementDao.getModuleItemStatus(moduleCode, moduleItemKey);
		for (CustomDataElementUsage usage : usages) {
			Integer answerVersion = customDataElementDao.getAnswerVersion(usage.getCustomDataElement().getColumnId(),
					moduleCode, moduleItemKey);
			if ((moduleCode.equals(Constants.DEV_PROPOSAL_MODULE_CODE) && statusCode.equals(Constants.PROPOSAL_STATUS_CODE_IN_PROGRESS)
					|| (moduleCode.equals(Constants.MODULE_CODE_AWARD) && statusCode.toString().equals(Constants.AWARD_WORKFLOW_STATUS_DRAFT))
					|| (moduleCode.equals(Constants.MODULE_CODE_AWARD) && statusCode.toString().equals(Constants.AWARD_WORKFLOW_STATUS_REVISION_REQUESTED)))
					|| answerVersion == null) {
				if (usage.getCustomDataElement().getIsLatestVesrion().equals("Y") && usage.getCustomDataElement().getIsActive().equals("Y")) {
					customDataResponses.add(setResponseObject(usage, moduleCode, moduleItemKey));
				}
			} else {
				if (answerVersion != null) {
					Integer answeredVersion = customDataElementDao.getAnsweredCustomElementId(usage.getCustomDataElement().getColumnId(), moduleItemKey, answerVersion, moduleCode);
					if (usage.getCustomDataElement().getCustomElementId().equals(answeredVersion)) {
						customDataResponses.add(setResponseObject(usage, moduleCode, moduleItemKey));
					}
				}
			}
		}
		return customDataResponses;
	}

	@Override
	public String getApplicableModules() {
		CustomDataElementVO vo = new CustomDataElementVO();
		vo.setApplicableModules(commonDao.getModules());
		return commonDao.convertObjectToJSON(vo);
	}

	public CustomDataElementVO getCustomOptions(CustomDataElementVO vo) {
		List<CustomDataElementOption> customOptions = customDataElementDao.getCustomOptions(vo.getCustomDataElement().getCustomElementId());
		for (CustomDataElementOption option : customOptions) {
			option.setAcType("U");
		}
		vo.setElementOptions(customOptions);
		return vo;
	}

	public List<CustomDataElementOption> maintainOptions(CustomDataElementVO vo) {
		List<CustomDataElementOption> options = new ArrayList<>();
		for (CustomDataElementOption option : vo.getElementOptions()) {
//			This code is intentionally commented till finalize the version criteria - by Arjun
//			if (Boolean.TRUE.equals(vo.getIsDataChange()) && option.getOptionName() != null) {
//				option.setCustomDataOptionId(null);
				option.setCustomDataElementsId(vo.getCustomDataElement().getCustomElementId());
				option = customDataElementDao.saveOrUpdateElementOptions(option);
				options.add(option);
//			}
		}
		return options;
	}

	public List<CustomData> setAnswerObject(CustomDataResponse customDataResponse, List<CustomData> answers) {
		Integer optionCount = 1;
		if (!customDataResponse.getOptions().isEmpty() && customDataResponse.getDataType().equals("4")) {
			optionCount = customDataResponse.getOptions().size();
		}
		for (int count = (!answers.isEmpty()) ? answers.size() : 0; count < optionCount; count++) {
			CustomData custData = new CustomData();
			if (customDataResponse.getDataType().equals(Constants.CUSTOM_DATA_TYPE_CHECKBOX)) {
				custData.setValue("false");
			}
			answers.add(custData);
		}
		return answers;
	}

	public void deleteOptions(List<CustomDataElementOption> customOptions) {
		for (CustomDataElementOption option : customOptions) {
			customDataElementDao.deleteCustomOption(option);
		}
	}

	public CustomDataResponse setResponseObject(CustomDataElementUsage usage, Integer moduleCode, String moduleItemKey) {
		Integer customElementId = usage.getCustomDataElement().getCustomElementId();
		CustomDataResponse customDataResponse = new CustomDataResponse();
		customDataResponse.setIsRequired(usage.getIsRequired());
		customDataResponse.setColumnName(usage.getCustomDataElement().getColumnLabel());
		customDataResponse.setCustomDataElementId(usage.getCustomDataElement().getCustomElementId());
		customDataResponse.setDataType(usage.getCustomDataElement().getDataType());
		customDataResponse.setDefaultValue(usage.getCustomDataElement().getDefaultValue());
		customDataResponse.setModuleItemCode(moduleCode);
		customDataResponse.setModuleItemKey(moduleItemKey);
		customDataResponse.setColumnId(usage.getCustomDataElement().getColumnId());
		customDataResponse.setVersionNumber(usage.getCustomDataElement().getColumnVersionNumber());
		customDataResponse.setDataLength(usage.getCustomDataElement().getDataLength());
		customDataResponse.setLookupWindow(usage.getCustomDataElement().getLookupWindow());
		customDataResponse.setLookupArgument(usage.getCustomDataElement().getLookupArgument());
		customDataResponse.setOptions(customDataElementDao.getCustomDataOptions(customElementId));
		customDataResponse.setFilterType(usage.getCustomDataElement().getCustomDataTypes().getDescription());
		customDataResponse.setOrderNumber(usage.getOrderNumber());
		List<CustomData> answers = customDataElementDao.getCustomDataAnswers(moduleCode, moduleItemKey, customElementId);
		answers = setAnswerObject(customDataResponse, answers);
		customDataResponse.setAnswers(answers);
		return customDataResponse;
	}

	@Override
	public boolean isOtherInformationPresent(Integer moduleCode) {
		List<CustomDataElementUsage> usages = customDataElementDao.getApplicableCustomElement(moduleCode);
		boolean expression = false;
		if (usages != null && !usages.isEmpty()) {
			expression =  true;
		}
		return expression;
	}

	@Override
	public String getSystemLookupByCustomType(CustomDataElementVO vo) {
		List<LookupWindow> lookupWindows = customDataElementDao.getSystemLookupByCustomType(vo.getDataTypeCode());
		List<LookUp> lookups = new ArrayList<>();
		lookupWindows.forEach(lookupWindow -> {
			LookUp lookup = new LookUp();
			if (lookupWindow.getDataTypeCode().equals("9")) {
				List<String> userLookups = commonDao.getUserDeffinedLookup();
				userLookups.forEach(userLookup -> {
					LookUp userDefinedLookup = new LookUp();
					userDefinedLookup.setCode(lookupWindow.getTableName() + "#" + userLookup);
					userDefinedLookup.setDescription(userLookup);
					lookups.add(userDefinedLookup);
				});
			} else if (lookupWindow.getDataTypeCode().equals("8")){
				lookup.setCode(lookupWindow.getTableName() + "#" + lookupWindow.getColumnName());
				lookup.setDescription(lookupWindow.getDescription());
				lookups.add(lookup);
			}
			else{
				lookup.setCode(lookupWindow.getColumnName());
				lookup.setDescription(lookupWindow.getDescription());
				lookups.add(lookup);
			}
		});
		vo.setLookUps(lookups);
		return commonDao.convertObjectToJSON(vo);
	}

	private void setDocumentUpdateUserAndTimestamp(Integer moduleCode, Integer moduleItemKey, String updateUser) {
		if (moduleCode.equals(Constants.MODULE_CODE_AWARD)) {
			awardService.updateAwardDocumentUpdateUserAndTimestamp(moduleItemKey, updateUser);
		}
	}

	@Override
	public void copyCustomData(String subModuleItemKey, Integer subModuleCode, String baseModuleItemKey, Integer baseModuleCode, String updateUser) {
		List<CustomDataElementUsage> usages = customDataElementDao.getCommonCustomElements(subModuleCode, baseModuleCode);
		if (usages != null && !usages.isEmpty()) {
			for (CustomDataElementUsage usage : usages) {
				List<CustomData> answers = new ArrayList<>();
				List<CustomData> awardAnswers = new ArrayList<>();
				Integer answerVersion = customDataElementDao.getAnswerVersion(usage.getCustomDataElement().getColumnId(), baseModuleCode, baseModuleItemKey);
				if (answerVersion != null) {
					Integer customElementId = customDataElementDao.getAnsweredCustomElementId(usage.getCustomDataElement().getColumnId(), baseModuleItemKey, answerVersion, baseModuleCode);
					if (usage.getCustomDataElement().getCustomElementId().equals(customElementId)) {
						answers = customDataElementDao.getAllCustomDataAnswers(baseModuleCode, baseModuleItemKey, usage.getCustomDataElement().getCustomElementId());
						awardAnswers = customDataElementDao.getAllCustomDataAnswers(subModuleCode, subModuleItemKey, usage.getCustomDataElement().getCustomElementId());
					}
				}
				if (answers != null && !answers.isEmpty()) {
					prepareAndSaveCustomData(answers, awardAnswers, subModuleItemKey, subModuleCode, updateUser, usage);
				}
			}
		}
	}

	private void prepareAndSaveCustomData(List<CustomData> answers, List<CustomData> awardAnswers, String subModuleItemKey, Integer subModuleCode, String updateUser, CustomDataElementUsage usage) {
		boolean isAnswerExist = false;
		if (usage.getCustomDataElement().getDataType().equals(Constants.CUSTOM_DATA_TYPE_CHECKBOX)) {
			isAnswerExist = chechWhetherAnswerExist(awardAnswers);
		}
		for (CustomData answer : answers) {
			if (awardAnswers.isEmpty()) {
				createAndSaveCustomData(answer, subModuleItemKey, subModuleCode, updateUser, usage.getCustomDataElement().getDefaultValue(), usage.getCustomDataElement().getDataType());
			} else {
				if (!isAnswerExist) {
					for (CustomData awardAnswer : awardAnswers) {
						if (answers.indexOf(answer) == (awardAnswers.indexOf(awardAnswer))) {
							updateAndSaveCustomData(awardAnswer, answer, subModuleItemKey, subModuleCode, updateUser);
						}
					}
				}
			}
		}
	}

	private boolean chechWhetherAnswerExist(List<CustomData> awardAnswers) {
		for (CustomData awardAnswer : awardAnswers) {
			if (awardAnswer.getValue().equals(CHECKBOX_ANSWER_VALUE)) {
				return true;
			}
		}
		return false;
	}

	private void createAndSaveCustomData(CustomData answer, String subModuleItemKey, Integer subModuleCode, String updateUser, String defaultValue, String dataType) {
		CustomData customData = new CustomData();
		if (answer.getValue() == null && dataType.equals("1") && dataType.equals("2")) {
			customData.setValue(defaultValue);
		} else {
			customData.setValue(answer.getValue());
		}
		customData.setDescription(answer.getDescription());
		customData.setCustomDataElementsId(answer.getCustomDataElementsId());
		customData.setModuleItemCode(subModuleCode);
		if (subModuleItemKey != null) {
			customData.setModuleItemKey(subModuleItemKey);
		}
		customData.setModuleSubItemCode(0);
		customData.setModuleSubItemKey("0");
		customData.setColumnId(answer.getColumnId());
		customData.setVersionNumber(answer.getVersionNumber());
		customData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		customData.setUpdateUser(updateUser);
		customDataElementDao.saveOrUpdateCustomResponse(customData);
	}

	private void updateAndSaveCustomData(CustomData awardAnswer, CustomData answer, String subModuleItemKey, Integer subModuleCode, String updateUser) {
		awardAnswer.setValue(answer.getValue());
		awardAnswer.setDescription(answer.getDescription());
		awardAnswer.setCustomDataElementsId(answer.getCustomDataElementsId());
		awardAnswer.setModuleItemCode(subModuleCode);
		if (subModuleItemKey != null) {
			awardAnswer.setModuleItemKey(subModuleItemKey);
		}
		awardAnswer.setModuleSubItemCode(0);
		awardAnswer.setModuleSubItemKey("0");
		awardAnswer.setColumnId(answer.getColumnId());
		awardAnswer.setVersionNumber(answer.getVersionNumber());
		awardAnswer.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardAnswer.setUpdateUser(updateUser);
		customDataElementDao.saveOrUpdateCustomResponse(awardAnswer);
	}

	@Override
	public void copyCustomDataBasedOnModule(Integer orginalModuleItemKey, Integer copiedModuleItemKey, Integer moduleCode, Integer subModuleCode, String subModuleItemKey) {
		List<CustomData> customDatas = customDataElementDao.fetchCustomDataByParams(moduleCode, orginalModuleItemKey, subModuleCode, subModuleItemKey);
		if (customDatas != null && !customDatas.isEmpty()) {
			copyCustomDatas(copiedModuleItemKey, customDatas, AuthenticatedUser.getLoginUserName());
		}
	}

	private void copyCustomDatas(Integer moduleCode, List<CustomData> originalCustomDatas, String updateUser) {
		for (CustomData copiedCustomData : originalCustomDatas) {
			CustomData customData = new CustomData();
			customData.setCustomDataElementsId(copiedCustomData.getCustomDataElementsId());
			customData.setModuleItemCode(copiedCustomData.getModuleItemCode());
			customData.setModuleItemKey(moduleCode.toString());
			customData.setModuleSubItemCode(copiedCustomData.getModuleSubItemCode());
			customData.setValue(copiedCustomData.getValue());
			customData.setColumnId(copiedCustomData.getColumnId());
			customData.setModuleSubItemKey(copiedCustomData.getModuleSubItemKey());
			customData.setVersionNumber(copiedCustomData.getVersionNumber());
			customData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			customData.setUpdateUser(updateUser);
			customData.setDescription(copiedCustomData.getDescription());
			customDataElementDao.saveOrUpdateCustomResponse(customData);
		}
	}

	@Override
	public ResponseEntity<ResponseData> getCustomElements(Integer moduleCode) {
		try {
			List<CustomDataElementUsage> usages = customDataElementDao.getApplicableCustomElement(moduleCode);
			List<CustomDataResponse> customDataResponses = new ArrayList<>();
			usages.stream().forEach(custElement -> {
				CustomDataResponse customDataResponse = new CustomDataResponse();
				customDataResponse.setIsRequired(custElement.getIsRequired());
				customDataResponse.setColumnName(custElement.getCustomDataElement().getColumnLabel());
				customDataResponse.setCustomDataElementId(custElement.getCustomDataElement().getCustomElementId());
				customDataResponse.setDataType(custElement.getCustomDataElement().getDataType());
				customDataResponse.setDefaultValue(custElement.getCustomDataElement().getDefaultValue());
				customDataResponse.setModuleItemCode(moduleCode);
				customDataResponse.setColumnId(custElement.getCustomDataElement().getColumnId());
				customDataResponse.setVersionNumber(custElement.getCustomDataElement().getColumnVersionNumber());
				customDataResponse.setDataLength(custElement.getCustomDataElement().getDataLength());
				customDataResponse.setLookupWindow(custElement.getCustomDataElement().getLookupWindow());
				customDataResponse.setLookupArgument(custElement.getCustomDataElement().getLookupArgument());
				customDataResponse.setOptions(customDataElementDao.getCustomDataOptions(custElement.getCustomDataElement().getCustomElementId()));
				customDataResponse.setFilterType(custElement.getCustomDataElement().getCustomDataTypes().getDescription());
				customDataResponse.setOrderNumber(custElement.getOrderNumber());
				customDataResponse.setIsActive(custElement.getCustomDataElement().getIsActive());
				customDataResponses.add(customDataResponse);
			});
			return new ResponseEntity<>(new ResponseData(customDataResponses, "Success", commonDao.getCurrentTimestamp(),
					true), HttpStatus.OK);
		} catch(Exception e) {
			logger.error("getCustomElements {}", e.getMessage());
			return new ResponseEntity<>(new ResponseData(null, "Unable to fetch data", commonDao.getCurrentTimestamp(),
					false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public ResponseEntity<ResponseData> saveCustomElementOrder(List<Map<String, Integer>> cusEleOrderList, Integer moduleCode) {
		try {
			cusEleOrderList.forEach(map -> customDataElementDao.saveCustomElementOrderNumber( map.get("customElementId"), map.get("orderNumber"), moduleCode));
			return new ResponseEntity<>(new ResponseData(null, "Custom data saved successfully", commonDao.getCurrentTimestamp(),
					true), HttpStatus.OK);
		} catch(Exception e) {
			logger.error("saveCustomElementOrder {}", e.getMessage());
			return new ResponseEntity<>(new ResponseData(null, "Unable to save data", commonDao.getCurrentTimestamp(),
					false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public ResponseEntity<ResponseData> updateCustomElementRequired(Integer elementId, Integer moduleCode) {
		try {
			customDataElementDao.updateCustomElementRequired(elementId, moduleCode);
			return new ResponseEntity<>(new ResponseData(null, "Updated successfully", commonDao.getCurrentTimestamp(),
					true), HttpStatus.OK);
		} catch(Exception e) {
			logger.error("updateCustomElementRequired {}", e.getMessage());
			return new ResponseEntity<>(new ResponseData(null, "Unable to update data", commonDao.getCurrentTimestamp(),
					false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@Override
	public ResponseEntity<ResponseData> getCusElementModules() {
		try {
			List<Module> moduleList = customDataElementDao.getCusElementModules();
			return new ResponseEntity<>(new ResponseData(moduleList, "Success", commonDao.getCurrentTimestamp(),
					true), HttpStatus.OK);
		} catch(Exception e) {
			logger.error("getCusElementModules {}", e.getMessage());
			return new ResponseEntity<>(new ResponseData(null, "Unable to fetch data", commonDao.getCurrentTimestamp(),
					false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
}
