package com.polus.formbuilder.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FormResponseDTO {

	private Integer formBuilderId;
	
	private String formBuilderNumber;
	
	private String moduleItemCode;
	
	private String moduleSubItemCode;
	
	private String moduleItemKey;
	
	private String moduleSubItemKey;
	
	private String formName;
	
	private List<FormBuilderSectionsDTO> formSections;
	
	private List<Integer> disabledSections;
	
	
}
