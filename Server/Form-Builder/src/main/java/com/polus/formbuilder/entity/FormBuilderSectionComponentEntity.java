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
import jakarta.persistence.Transient;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


@Entity
@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "FORM_BUILDER_SECTION_COMPONENT")
public class FormBuilderSectionComponentEntity implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "FORM_BUILDER_SECT_COMP_ID")
    private Integer formBuilderSectCompId;

    @Column(name = "FORM_BUILDER_SECTION_ID")
    private Integer sectionId;

    @Column(name = "COMPONENT_TYPE_CODE", nullable = false)
    private String componentTypeCode;

    @Column(name = "COMPONENT_ORDER_NUMBER", nullable = false)
    private Integer componentOrderNumber;

    @Column(name = "COMPONENT_DATA")
    private String componentData;

    @Column(name = "COMPONENT_REF_ID")
    private String componentRefId;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

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

    @Column(name = "IS_MANDATORY")
	private String isMandatory = "N";

    @Column(name = "VALIDATION_MESSAGE")
    private String validationMessage;

    @Column(name = "LABEL")
    private String label;

}
