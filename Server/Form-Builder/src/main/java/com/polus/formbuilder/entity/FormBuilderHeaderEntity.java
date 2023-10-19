package com.polus.formbuilder.entity;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "FORM_BUILDER_HEADER")
public class FormBuilderHeaderEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	    @Id
	    @GeneratedValue(strategy = GenerationType.IDENTITY)
	    @Column(name = "FORM_BUILDER_ID")
	    private Integer formBuilderId;

	    @Column(name = "FORM_BUILDER_NUMBER", nullable = false)
	    private String formBuilderNumber;

	    @Column(name = "VERSION_NUMBER", nullable = false)
	    private Integer versionNumber;

	    @Column(name = "VERSION_STATUS", nullable = false)
	    private String versionStatus;

	    @Column(name = "MODULE_CODE", nullable = false)
	    private String moduleCode;

	    @Column(name = "SUB_MODULE_CODE", nullable = false)
	    private String subModuleCode;

	    @Column(name = "BUSINESS_RULE_ID")
	    private Integer businessRuleId;

	    @Column(name = "DESCRIPTION", nullable = false)
	    private String description;

	    @Column(name = "CREATE_TIMESTAMP", nullable = false)
	    @Temporal(TemporalType.TIMESTAMP)
	    private Date createTimestamp;

	    @Column(name = "CREATE_USER", nullable = false)
	    private String createUser;

	    @Column(name = "UPDATE_TIMESTAMP", nullable = false)
	    @Temporal(TemporalType.TIMESTAMP)
	    private Date updateTimestamp;

	    @Column(name = "UPDATE_USER", nullable = false)
	    private String updateUser;
	
	  
}
