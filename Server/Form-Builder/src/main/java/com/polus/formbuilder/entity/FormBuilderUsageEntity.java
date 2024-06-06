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
@Table(name = "FORM_BUILDER_USAGE")
public class FormBuilderUsageEntity implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "FORM_USAGE_ID")
    private Integer formUsageId;

    @Column(name = "FORM_BUILDER_ID", nullable = false)
    private int formBuilderId;

    @Column(name = "FORM_BUILDER_NUMBER", nullable = false)
    private String formBuilderNumber;

    @Column(name = "FORM_ORDER_NUMBER", nullable = false)
    private int formOrderNumber;

    @Column(name = "MODULE_CODE", nullable = false)
    private String moduleCode;

    @Column(name = "SUB_MODULE_CODE", nullable = false)
    private String subModuleCode;

    @Column(name = "BUSINESS_RULE_ID")
    private Integer businessRuleId;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

    @Column(name = "IS_ACTIVE", nullable = false)
    private String isActive;

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
