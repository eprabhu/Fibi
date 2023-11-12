package com.polus.formbuilder.formconfig.v1.model;

import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FormSectionModel {
	
	 	private int formBuilderSectionId;
	    private int formBuilderId;
	    private String sectionName;
	    private Integer sectionOrderNumber;
	    private Integer businessRuleId;
	    private String description;
	    private String helpText;
	    private String headerInstruction;
	    private String footerInstruction;
	    private String isActive;
	    private Date updateTimestamp;
	    private String updateUser;
	    private List<FormSectionComponentModel> sectionComponents;
}
