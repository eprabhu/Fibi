package com.polus.fibicomp.globalentity.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@Table(name = "ENTITY_FAMILY_TREE")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class EntityFamilyTree implements Serializable {

	private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "ENTITY_FAMILY_TREE_ID")
    private int entityFamilyTreeId;

    @Column(name = "ENTITY_ID")
    private int entityId;

    @ManyToOne(optional = true)
    @JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FAMILY_TREE_FK1"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
    private GlobalEntity entity;

    @Column(name = "PARENT_ENTITY_ID")
    private int parentEntityId;

    @ManyToOne(optional = true)
    @JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FAMILY_TREE_FK2"), name = "PARENT_ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
    private GlobalEntity parentEntity;

    @Column(name = "GLOBAL_ULTIMATE_ENTITY_NUMBER")
    private String globalUltimateEntityNumber;

    @Column(name = "HIERARCHY_LEVEL")
    private String hierarchyLevel;

    @Column(name = "UPDATED_BY")
    private String updatedBy;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

}
