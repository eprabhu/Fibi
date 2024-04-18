package com.polus.formbuilder.formconfig.v1.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.formbuilder.formconfig.v1.dao.FormBuilderConfigurationDAO;
import com.polus.formbuilder.formconfig.v1.model.FormDataResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderCreateModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderUpdateModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionComponentModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionModel;
import com.polus.formbuilder.formconfig.v1.model.FormSectionRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormUsageModel;
import com.polus.formbuilder.formconfig.v1.model.FormUsageRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormComponentRequestModel;
import com.polus.formbuilder.formconfig.v1.model.FormDashboardResponseModel;

@Service
public class FormBuilderConfigurationService {

	@Autowired
	FormBuilderConfigurationDAO dao;
	
	public List<FormDashboardResponseModel> fetchFormList() {
		return dao.fetchAllForms();
	}

	public FormDataResponseModel fetchFormById(Integer formBuilderId) {
		return dao.fetchFormById(formBuilderId);
	}

	public FormDataResponseModel copyForm(Integer formBuilderId) {
		 Integer newFormBuilderId = dao.copyFormById(formBuilderId);
		 return dao.fetchFormById(newFormBuilderId);
	}

	public String deleteForm(int formBuilderId) {		
		return dao.deleteForm(formBuilderId);
	}

	public FormHeaderModel createHeader(FormHeaderCreateModel request) {		
		return dao.createFormHeader(request);
	}

	public FormHeaderModel updateHeader(FormHeaderUpdateModel request) {
		return dao.updateFormHeader(request);
	}

	public String deleteHeader(Integer formBuilderId) {
		return dao.deleteHeader(formBuilderId);
	}

	public FormHeaderModel fetchFormHeader(Integer formBuilderId) {
		return dao.fetchFormHeader(formBuilderId);
	}

	public FormUsageModel createFormUsage(FormUsageRequestModel request) {
		return dao.createFormUsage(request);
	}

	public FormUsageModel updateFormUsage(FormUsageRequestModel request) {
		return dao.updateFormUsage(request);
	}

	public String deleteFormUsage(Integer formUsageId) {
		return dao.deleteFormUsage(formUsageId);
	}

	public List<FormUsageModel> fetchFormUsage(Integer formBuilderId) {
		return dao.fetchFormUsage(formBuilderId);
	}

	public String updateUsageOrder(List<FormUsageRequestModel> request) {
		return dao.updateUsageOrder(request);
	}

	public List<FormSectionModel> fetchAllFormSection(Integer formBuilderId) {
		return dao.fetchAllFormSection(formBuilderId);
	}

	public FormSectionModel createFormSection(FormSectionRequestModel request) {
		return dao.createFormSection(request);
	}

	public FormSectionModel updateFormSection(FormSectionRequestModel request) {
		return dao.updateFormSection(request);
	}

	public String updateSectionOrder(List<FormSectionRequestModel> request) {
		return dao.updateSectionOrder(request);
	}

	public String deleteFormSection(int formBuilderSectionId) {
		return dao.deleteFormSection(formBuilderSectionId);
	}
	
	public FormSectionModel fetchFormSection(Integer formBuilderSectionId) {
		return dao.fetchFormSection(formBuilderSectionId);
	}

		
	public List<FormSectionComponentModel> fetchAllFormComponent(Integer formBuilderSectionId) {
		return dao.fetchAllFormComponent(formBuilderSectionId);
	}

	public FormSectionComponentModel fetchFormComponent(Integer formBuilderSectCompId) {
		return dao.fetchFormComponent(formBuilderSectCompId);
	}

	public FormSectionComponentModel createFormComponent(FormComponentRequestModel request) {
		return dao.createFormComponent(request);
	}

	public FormSectionComponentModel updateFormComponent(FormComponentRequestModel request) {
		return dao.updateFormComponent(request);
	}

	public String deleteFormComponent(int formBuilderSectCompId) {
		return dao.deleteFormComponent(formBuilderSectCompId);
	}

	public String deleteFormComponentBySectionId(int formBuilderSectId) {
		return dao.deleteFormComponentBySectionId(formBuilderSectId);
	}

	public String updateComponentOrder(List<FormComponentRequestModel> request) {
		return dao.updateComponentOrder(request);
	}

}
