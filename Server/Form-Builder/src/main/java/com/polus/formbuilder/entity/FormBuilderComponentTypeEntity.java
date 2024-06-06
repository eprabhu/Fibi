package com.polus.formbuilder.entity;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
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
@Table(name = "FORM_SECTION_COMPONENT_TYPE")
public class FormBuilderComponentTypeEntity implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "COMPONENT_TYPE_CODE", nullable = false)
    private String componentTypeCode;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

    @Column(name = "IS_ACTIVE", nullable = false)
    private String isActive;

    @Column(name = "UPDATE_TIMESTAMP", nullable = false)
    @Temporal(TemporalType.TIMESTAMP)
    private Date updateTimestamp;

    @Column(name = "UPDATE_USER", nullable = false)
    private String updateUser;
    
}
