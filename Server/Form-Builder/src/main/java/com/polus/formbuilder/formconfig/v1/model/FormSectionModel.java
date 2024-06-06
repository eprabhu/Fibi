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
	
	 	private int sectionId;
	    private int formBuilderId;
	    private String sectionName;
	    private Integer sectionOrder;
	    private Integer sectionBusinessRule;
	    private String sectionDescription;
	    private String sectionHelpText;
	    private String sectionHeader;
	    private String sectionFooter;
	    private String isActive;
	    private Date updateTimestamp;
	    private String updateUser;
	    private List<FormSectionComponentModel> sectionComponent;

}
