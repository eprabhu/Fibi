package com.polus.fibicomp.coi.pojo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.sql.Timestamp;

@Entity
@Table(name = "ENTITY_ACTION_LOG")
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class EntityActionLog {

    @Id
    @Column(name = "ACTION_LOG_ID")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer actionLogId;

    @Column(name = "ENTITY_ID")
    private Integer entityId;

    @Column(name = "ENTITY_NUMBER")
    private Integer entityNumber;

    @ManyToOne(cascade = CascadeType.REFRESH)
    @JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_ACTION_LOG_FK1"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
    private CoiEntity coiEntity;

    @Column(name = "ACTION_TYPE_CODE")
    private String actionTypeCode;

    @ManyToOne(cascade = CascadeType.REFRESH)
    @JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_ACTION_LOG_FK2"), name = "ACTION_TYPE_CODE", referencedColumnName = "ACTION_TYPE_CODE", insertable = false, updatable = false)
    private EntityActionType entityActionType;

    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "COMMENT")
    private String comment;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;
}
