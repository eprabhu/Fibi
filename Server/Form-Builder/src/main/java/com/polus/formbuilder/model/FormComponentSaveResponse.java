package com.polus.formbuilder.model;

import java.util.List;

import com.polus.formbuilder.dto.FormBuilderSectionsComponentDTO;

import lombok.Data;

@Data
public class FormComponentSaveResponse extends FormBuilderSectionsComponentDTO{
	
	private Integer formBuilderId;
	
	private String moduleItemCode;
	
	private String moduleSubItemCode;
	
	private String moduleItemKey;
	
	private String moduleSubItemKey;
	
	private List<Integer> disabledSections;
	
}
