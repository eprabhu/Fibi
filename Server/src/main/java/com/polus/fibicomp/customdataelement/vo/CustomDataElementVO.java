package com.polus.fibicomp.customdataelement.vo;

import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.customdataelement.pojo.CustomData;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementDataType;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementOption;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElements;
import com.polus.fibicomp.customdataelement.pojo.LookupWindow;
import com.polus.fibicomp.pojo.Module;
import com.polus.fibicomp.vo.LookUp;

public class CustomDataElementVO {
	
	private CustomDataElements customDataElement;
	
	private List<CustomDataElements> customDataElements;
	
	private String responseMessage;
	
	private Integer customDataElementId;
	
	private List<CustomDataElementDataType> customDataTypes;
	
	private List<CustomDataElementOption> elementOptions;
		
	private List<CustomData> customResponses;
	
	private Integer moduleCode;
	
	private List<CustomDataResponse> customElements;
	
	private String updateUser;
	
	private Timestamp updateTimestamp;
	
	private Integer moduleItemKey;
	
	private List<Module> applicableModules;
	
	private List<CustomDataElementOption> deleteOptions;
	
	private Boolean isDataChange;

	private String dataTypeCode;

	private List<LookupWindow> systemLookups;

	private List<LookUp> lookUps;

	public CustomDataElements getCustomDataElement() {
		return customDataElement;
	}

	public void setCustomDataElement(CustomDataElements customDataElement) {
		this.customDataElement = customDataElement;
	}

	public List<CustomDataElements> getCustomDataElements() {
		return customDataElements;
	}

	public void setCustomDataElements(List<CustomDataElements> customDataElements) {
		this.customDataElements = customDataElements;
	}

	public String getResponseMessage() {
		return responseMessage;
	}

	public void setResponseMessage(String responseMessage) {
		this.responseMessage = responseMessage;
	}

	public Integer getCustomDataElementId() {
		return customDataElementId;
	}

	public void setCustomDataElementId(Integer customDataElementId) {
		this.customDataElementId = customDataElementId;
	}

	public List<CustomDataElementDataType> getCustomDataTypes() {
		return customDataTypes;
	}

	public void setCustomDataTypes(List<CustomDataElementDataType> customDataTypes) {
		this.customDataTypes = customDataTypes;
	}

	public List<CustomDataElementOption> getElementOptions() {
		return elementOptions;
	}

	public void setElementOptions(List<CustomDataElementOption> elementOptions) {
		this.elementOptions = elementOptions;
	}

	public List<CustomData> getCustomResponses() {
		return customResponses;
	}

	public void setCustomResponses(List<CustomData> customResponses) {
		this.customResponses = customResponses;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public List<CustomDataResponse> getCustomElements() {
		return customElements;
	}

	public void setCustomElements(List<CustomDataResponse> customElements) {
		this.customElements = customElements;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public Integer getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(Integer moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public List<Module> getApplicableModules() {
		return applicableModules;
	}

	public void setApplicableModules(List<Module> applicableModules) {
		this.applicableModules = applicableModules;
	}

	public List<CustomDataElementOption> getDeleteOptions() {
		return deleteOptions;
	}

	public void setDeleteOptions(List<CustomDataElementOption> deleteOptions) {
		this.deleteOptions = deleteOptions;
	}

	public Boolean getIsDataChange() {
		return isDataChange;
	}

	public void setIsDataChange(Boolean isDataChange) {
		this.isDataChange = isDataChange;
	}

	public String getDataTypeCode() {
		return dataTypeCode;
	}

	public void setDataTypeCode(String dataTypeCode) {
		this.dataTypeCode = dataTypeCode;
	}

	public List<LookupWindow> getSystemLookups() {
		return systemLookups;
	}

	public void setSystemLookups(List<LookupWindow> systemLookups) {
		this.systemLookups = systemLookups;
	}

	public List<LookUp> getLookUps() {
		return lookUps;
	}

	public void setLookUps(List<LookUp> lookUps) {
		this.lookUps = lookUps;
	}

}
