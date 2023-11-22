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
public class FormHeaderModel {
	
	    private int formBuilderId;
	    private String formBuilderNumber;
	    private Integer versionNumber;
	    private String versionStatus;
	    private String title;
	    private String description;
	    private String isActive;
	    private Date createTimestamp;
	    private String createUser;
	    private Date updateTimestamp;
	    private String updateUser;
	    private List<FormUsageModel> usages;
	    private List<FormSectionModel> sections;
}
