
package com.polus.formbuilder.customdataelement.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.appcorelib.pojo.LookupWindow;
import com.polus.appcorelib.vo.LookUp;
import com.polus.formbuilder.customdataelement.VO.CustomDataElementVO;
import com.polus.formbuilder.customdataelement.VO.CustomDataResponse;
import com.polus.formbuilder.customdataelement.dao.CustomDataElementDao;
import com.polus.formbuilder.customdataelement.pojo.CustomData;
import com.polus.formbuilder.customdataelement.pojo.CustomDataElementOption;
import com.polus.formbuilder.customdataelement.pojo.CustomDataElements;
import com.polus.formbuilder.service.FormBuilderConstants;

import jakarta.transaction.Transactional;


@Transactional
@Service
public class CustomDataElementServiceImpl implements CustomDataElementService {

	protected static Logger logger = LogManager.getLogger(CustomDataElementServiceImpl.class.getName());

	@Autowired
	private CustomDataElementDao customDataElementDao;

	@Override
	public ResponseEntity<Object> configureCustomElement(CustomDataElementVO vo) {
		CustomDataElements customDataElement = vo.getCustomDataElement();
		if (customDataElement.getAcType().equals(FormBuilderConstants.INSERT)) {
			vo = saveOrUpdateCustomElements(vo);
		} else if (customDataElement.getAcType().equals(FormBuilderConstants.UPDATE)) {
			if (vo.getDeleteOptions() != null && !vo.getDeleteOptions().isEmpty()) {
				deleteOptions(vo.getDeleteOptions());
			}
			customDataElement = customDataElementDao.saveOrUpdateCustomElement(customDataElement);
			vo.setCustomDataElement(customDataElement);
			if (customDataElement.getDataType().equals(FormBuilderConstants.CHECK_BOX) || customDataElement.getDataType().equals(FormBuilderConstants.RADIO_BUTTON)) {
				vo.setElementOptions(maintainOptions(vo));
			}
			vo.setResponseMessage("Custom Data Element updated successfully.");
		}
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Object> fetchAllCustomElement(CustomDataElementVO vo) {
		vo.setCustomDataElements(customDataElementDao.fetchAllCustomElements());
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

	@Override
	public CustomDataElementVO fetchCustomElementById(CustomDataElementVO vo) {
		vo.setCustomDataElement(customDataElementDao.fetchCustomElementById(vo.getCustomDataElementId()));
		List<CustomDataResponse> customDataResponses = new ArrayList<>();
		customDataResponses.add(setResponseObject(vo));
		vo.setCustomElements(customDataResponses);
		vo = getCustomOptions(vo);
		return vo;
	}

	private CustomDataResponse setResponseObject(CustomDataElementVO vo) {
		CustomDataElements customDataElement = vo.getCustomDataElement();
		Integer customElementId = vo.getCustomDataElement().getCustomElementId();
		CustomDataResponse customDataResponse = new CustomDataResponse();
		customDataResponse.setColumnName(customDataElement.getColumnLabel());
		customDataResponse.setCustomDataElementId(customDataElement.getCustomElementId());
		customDataResponse.setDataType(customDataElement.getDataType());
		customDataResponse.setDefaultValue(customDataElement.getDefaultValue());
		customDataResponse.setModuleItemKey(vo.getModuleItemKey());
		customDataResponse.setDataLength(customDataElement.getDataLength());
		customDataResponse.setLookupWindow(customDataElement.getLookupWindow());
		customDataResponse.setLookupArgument(customDataElement.getLookupArgument());
		customDataResponse.setOptions(customDataElementDao.getCustomDataOptions(customElementId));
		customDataResponse.setFilterType(customDataElement.getCustomDataTypes().getDescription());
		customDataResponse.setIsMultiSelectLookup(customDataElement.getIsMultiSelectLookup());
		List<CustomData> answers = customDataElementDao.getCustomDataAnswers(vo.getModuleItemKey(), vo.getModuleCode(),
				vo.getModuleSubItemKey(), vo.getSubModuleCode(), customElementId);
		answers = setAnswerObject(customDataResponse, answers);
		customDataResponse.setAnswers(answers);
		return customDataResponse;
	}

	public CustomDataElementVO saveOrUpdateCustomElements(CustomDataElementVO vo) {
		CustomDataElements customDataElement = vo.getCustomDataElement();
		customDataElement = customDataElementDao.saveOrUpdateCustomElement(customDataElement);
		customDataElement.setAcType(FormBuilderConstants.UPDATE);
		vo.setCustomDataElement(customDataElement);
		if ((customDataElement.getDataType().equals(FormBuilderConstants.CHECK_BOX)
				|| customDataElement.getDataType().equals(FormBuilderConstants.RADIO_BUTTON))
				&& (vo.getElementOptions() != null && !vo.getElementOptions().isEmpty())) {
			for (CustomDataElementOption elementOption : vo.getElementOptions()) {
				elementOption.setCustomDataElementsId(customDataElement.getCustomElementId());
				if (elementOption.getOptionName() != null) {
					customDataElementDao.saveOrUpdateElementOptions(elementOption);
					elementOption.setAcType(FormBuilderConstants.UPDATE);
				}
			}
		}
		vo.setResponseMessage("Custom Data Element saved successfully.");
		return vo;
	}

	@Override
	public CustomDataElementVO saveCustomResponse(CustomDataElementVO vo) {
		if (vo.getCustomElements() != null && !vo.getCustomElements().isEmpty()) {
			for (CustomDataResponse customElement : vo.getCustomElements()) {
				for (CustomData answer : customElement.getAnswers()) {
					if (canSaveTheCustomDataResponse(answer, customElement)) {
						CustomData customData = new CustomData();
						if (answer.getCustomDataId() != null) {
							customData.setCustomDataId(answer.getCustomDataId());
						}
						if (answer.getValue() == null
								&& (customElement.getDataType().equals(FormBuilderConstants.STRING_ELEMENT)
										|| customElement.getDataType().equals(FormBuilderConstants.NUMBER_ELEMENT))) {
							customData.setValue(customElement.getDefaultValue());
						} else if (answer.getValue() != null) {
							customData.setValue(answer.getValue());
						}
						customData.setDescription(answer.getDescription());
						customData.setCustomDataElementsId(customElement.getCustomDataElementId());
						customData.setModuleItemCode(vo.getModuleCode());
						if (vo.getModuleItemKey() != null) {
							customData.setModuleItemKey(vo.getModuleItemKey());
						}
						customData.setModuleSubItemCode(vo.getSubModuleCode() != null ? vo.getSubModuleCode() : 0);
						customData.setModuleSubItemKey(vo.getModuleSubItemKey());
						customData.setUpdateTimestamp(vo.getUpdateTimestamp());
						customData.setUpdateUser(vo.getUpdateUser());
						if (customData.getCustomDataId() != null && (customData.getValue() == null || customData.getValue().isEmpty())) {
							customDataElementDao.deleteOptionResponse(answer.getCustomDataId());
							customData.setCustomDataId(null);
						} else if (customData.getValue() != null) {
							customDataElementDao.saveOrUpdateCustomResponse(customData);
						}
						answer.setCustomDataId(customData.getCustomDataId());
					}
				}
			}
		}
		return vo;
	}

	private boolean canSaveTheCustomDataResponse(CustomData answer, CustomDataResponse customElement) {
		return ((answer.getValue() != null && !answer.getValue().isEmpty())
				|| (customElement.getDefaultValue() != null && !customElement.getDefaultValue().isEmpty()
						&& (customElement.getDataType().equals(FormBuilderConstants.STRING_ELEMENT)
								|| customElement.getDataType().equals(FormBuilderConstants.NUMBER_ELEMENT)))
				|| answer.getCustomDataId() != null);
	}

	public CustomDataElementVO getCustomOptions(CustomDataElementVO vo) {
		List<CustomDataElementOption> customOptions = customDataElementDao.getCustomOptions(vo.getCustomDataElement().getCustomElementId());
		customOptions.forEach(option -> option.setAcType(FormBuilderConstants.UPDATE));
		vo.setElementOptions(customOptions);
		return vo;
	}

	public List<CustomDataElementOption> maintainOptions(CustomDataElementVO vo) {
		List<CustomDataElementOption> options = new ArrayList<>();
		for (CustomDataElementOption option : vo.getElementOptions()) {
			option.setCustomDataElementsId(vo.getCustomDataElement().getCustomElementId());
			option = customDataElementDao.saveOrUpdateElementOptions(option);
			options.add(option);
		}
		return options;
	}

	public List<CustomData> setAnswerObject(CustomDataResponse customDataResponse, List<CustomData> answers) {
		if (answers.isEmpty() && !customDataResponse.getDataType().equals(FormBuilderConstants.CHECK_BOX)) {
			answers.add(new CustomData());
		}
		return answers;
	}

	public void deleteOptions(List<CustomDataElementOption> customOptions) {
		for (CustomDataElementOption option : customOptions) {
			customDataElementDao.deleteCustomOption(option);
		}
	}

	@Override
	public ResponseEntity<Object> getSystemLookupByCustomType(CustomDataElementVO vo) {
		List<LookupWindow> lookupWindows = customDataElementDao.getSystemLookupByCustomType(FormBuilderConstants.CUSTOM_DATA_TYPE_CODES.get(vo.getDataTypeCode()));
		List<LookUp> lookups = new ArrayList<>();
		lookupWindows.forEach(lookupWindow -> {
			LookUp lookup = new LookUp();
			if (lookupWindow.getDataTypeCode().equals(FormBuilderConstants.USER_DROPDOWN)) {
				List<String> userLookups = customDataElementDao.getUserDeffinedLookup();
				userLookups.forEach(userLookup -> {
					LookUp userDefinedLookup = new LookUp();
					userDefinedLookup.setCode(lookupWindow.getTableName() + "#" + userLookup);
					userDefinedLookup.setDescription(userLookup);
					lookups.add(userDefinedLookup);
				});
			} else if (lookupWindow.getDataTypeCode().equals(FormBuilderConstants.SYSTEM_DROPDOWN)){
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
		return new ResponseEntity<>(vo, HttpStatus.OK);
	}

}
