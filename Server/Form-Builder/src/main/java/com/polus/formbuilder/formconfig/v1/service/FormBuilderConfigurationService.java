package com.polus.formbuilder.formconfig.v1.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.formbuilder.formconfig.v1.dao.FormBuilderConfigurationDAO;
import com.polus.formbuilder.formconfig.v1.model.FormDataResponseModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderCreateModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderModel;
import com.polus.formbuilder.formconfig.v1.model.FormHeaderUpdateModel;
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

}
