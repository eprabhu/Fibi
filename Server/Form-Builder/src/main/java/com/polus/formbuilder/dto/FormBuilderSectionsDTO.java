package com.polus.formbuilder.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FormBuilderSectionsDTO {

	private Integer sectionId;
	
	private String sectionName;
	
	private Integer sectionOrder;
	
	private String sectionDescription;
	
	private Integer sectionBusinessRule;
	
	private String sectionHelpText;
	
	private String sectionHeader;
	
	private String sectionFooter;
	
	private List<FormBuilderSectionsComponentDTO> sectionComponent;
}