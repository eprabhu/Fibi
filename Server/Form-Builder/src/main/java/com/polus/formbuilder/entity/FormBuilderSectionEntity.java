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
@Table(name = "FORM_BUILDER_SECTION")
public class FormBuilderSectionEntity implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "FORM_BUILDER_SECTION_ID")
    private Integer formBuilderSectionId;

    @Column(name = "FORM_BUILDER_ID")
    private Integer formHeaderId;

    @Column(name = "FORM_BUILDER_NUMBER", nullable = false)
    private String formBuilderNumber;

    @Column(name = "SECTION_NAME", nullable = false)
    private String sectionName;

    @Column(name = "SECTION_ORDER_NUMBER", nullable = false)
    private Integer sectionOrderNumber;

//    @Column(name = "MODULE_CODE", nullable = false)
//    private String moduleCode;
//
//    @Column(name = "SUB_MODULE_CODE", nullable = false)
//    private String subModuleCode;

    @Column(name = "BUSINESS_RULE_ID")
    private Integer businessRuleId;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

    @Column(name = "HELP_TEXT")
    private String helpText;

    @Column(name = "HEADER_INSTRUCTION")
    private String headerInstruction;

    @Column(name = "FOOTER_INSTRUCTION")
    private String footerInstruction;

    @Column(name = "IS_ACTIVE", nullable = false)
    private String isActive;

    @Column(name = "UPDATE_TIMESTAMP", nullable = false)
    @Temporal(TemporalType.TIMESTAMP)
    private Date updateTimestamp;

    @Column(name = "UPDATE_USER", nullable = false)
    private String updateUser;    
 
}