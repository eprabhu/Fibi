import { CompUnComp, CompUnCompPE } from "./PE-components/OPA-comp-uncomp/interface";
import { OPAInstituteResources, OPAInstituteResourcesPE } from "./PE-components/OPA-institute-resources/OPA-institute-resources.interface";
import { OutsideFinRelation, OutsideFinRelationPE } from "./PE-components/OPA-outside-financial-relation/interface";
import { StudentSubordinateEmployee, StudentSubordinatePE } from "./PE-components/OPA-student-subordinate-employee/interface";

export class FormBuilder {
    applicableFormsBuilderIds: number[];
    form: Form;
}

type EventTypes = ['EXTERNAL_SAVE', 'SAVE', 'SAVE_COMPLETE', 'SAVE_COMPLETED', 'CONFIGURATION', 'IS_EDIT_MODE' ,'BLANK_FORM'];

export class FormBuilderEvent {
    eventType: EventTypes[number];
    data?: any;
}

export class Form {
    formBuilderId: number;
    formBuilderNumber: string;
    moduleItemCode: string;
    moduleSubItemCode: string;
    moduleItemKey: string;
    moduleSubItemKey: string;
    formName: string;
    formSections: FormSection[] = [];
}

export class FormSection {
    sectionId: number;
    sectionName: string;
    sectionOrder: number;
    sectionDescription: string;
    sectionBusinessRule: any;
    sectionHelpText?: string;
    sectionHeader?: string;
    sectionFooter?: string;
    sectionComponent: SectionComponent[];
    validationType?: null | 'VE' | 'VW';
}

export class SectionComponent {
    componentId: number;
    sectionId: number;
    componentDescription: any;
    componentType: string;
    componentRefId?: string;
    componentData?: any;
    componentHeader?: string;
    componentFooter: any;
    programmedElement: any;
    questionnaire?: QuestionnaireVO;
    customElement?: CustomElementVO;
    componentOrder: number;
    isMandatory?: any;
    validationMessage?: any;
    label?: any;
    componentTypeDescription?:any;
}

export class QuestionnaireVO {
    applicableQuestionnaire: any;
    questionnaireId: number;
    moduleItemKey: string;
    moduleSubItemKey: string;
    moduleItemCode: number;
    moduleSubItemCode: number;
    questionnaireAnswerHeaderId: any;
    questionnaireAnsAttachmentId: any;
    questionnaireCompleteFlag: any;
    actionUserId: any;
    actionPersonId: any;
    actionPersonName: any;
    acType: any;
    questionnaireName: any;
    newQuestionnaireVersion: boolean;
    questionEditted: boolean;
    questionnaireList: any;
    questionnaireGroup: any;
    header: Header;
    questionnaire: Questionnaire;
    usage: any;
    fileName: any;
    fileContent: any;
    length: any;
    remaining: any;
    fileTimestamp: any;
    contentType: any;
    personId: any;
    multipartFile: any;
    moduleList: any;
    isInserted: any;
    updateTimestamp: any;
    copyModuleItemKey: any;
    questionnaireNumbers: any;
    lookUpDetails: any;
    newQuestionnaireId: any;
    moduleSubItemCodes: any[];
    questionnaireBusinessRules: any;
    ruleId: any;
    rulePassed: any;
    questionnaireMode: any;
    copyInActiveQuestionAnswers: boolean;
    files?: any[] = [];
}

export class Header {
    TRIGGER_POST_EVALUATION: any;
    QUESTIONNAIRE_VERSION: number;
    UPDATE_USER: string;
    ANS_UPDATE_TIMESTAMP: any;
    ANS_PERSON_FULL_NAME: any;
    QUESTIONNAIRE_DESCRIPTION: any;
    IS_FINAL: boolean;
    QUESTIONNAIRE_NUMBER: number;
    QUESTIONNAIRE_ID: number;
    QUEST_GROUP_TYPE_CODE: any;
    AC_TYPE: string;
    QUESTIONNAIRE_NAME: string;
    UPDATE_TIMESTAMP: string;
}

export class Questionnaire {
    maxGroupNumber: any;
    questions: Question[];
    conditions: Condition[];
    options: Option[];
    deleteList: any;
    questionnaireQuestions: any;
    questionnaireConditions: any;
    questionnaireOptions: any;
    questionnaireAnswers: any;
    quesAttachmentList: any;
    lookUpDetails: any;
}

export class Question {
    LOOKUP_TYPE: any;
    LOOKUP_FIELD: any;
    ANSWER_LENGTH: any;
    RULE_ID: any;
    GROUP_LABEL: any;
    UPDATE_USER: string;
    QUESTION_NUMBER: number;
    LOOKUP_NAME: any;
    HAS_CONDITION?: string;
    QUESTION: string;
    SHOW_QUESTION?: boolean;
    GROUP_NAME: string;
    HELP_LINK: any;
    QUESTION_VERSION_NUMBER: number;
    DESCRIPTION: any;
    SORT_ORDER: number;
    QUESTION_ID: number;
    ANSWERS?: any;
    ANSWER_TYPE: string;
    NO_OF_ANSWERS?: number;
    UPDATE_TIMESTAMP: string;
    PARENT_QUESTION_ID?: number;
}


export class Condition {
    QUESTION_CONDITION_ID: number;
    CONDITION_TYPE: string;
    GROUP_NAME: string;
    UPDATE_USER: string;
    QUESTION_ID: number;
    CONDITION_VALUE: string;
}

export class Option {
    EXPLANTION_LABEL: any;
    QUESTION_OPTION_ID: number;
    OPTION_NUMBER?: number;
    QUESTION_ID: number;
    OPTION_LABEL: string;
    REQUIRE_EXPLANATION?: string;
}

export class CustomElementVO {
    customDataElement: any;
    customDataElements: any;
    responseMessage: any;
    customDataElementId: any;
    customDataTypes: any;
    elementOptions: any;
    customResponses: any;
    moduleCode: any;
    customElements: CustomElement[];
    updateUser: any;
    updateTimestamp: any;
    moduleItemKey: any;
    applicableModules: any;
    deleteOptions: any;
    isDataChange: any;
    dataTypeCode: any;
    systemLookups: any;
    lookUps: any;
    elementAnswered: any;
    subModuleCode: any;
    subModuleItemKey: any;
}

export class CustomElement {
    customDataElementId: number;
    columnName: string;
    defaultValue: string;
    dataType: string;
    isRequired: string;
    options: Option2[];
    answers: any[];
    moduleItemCode: number;
    moduleItemKey: string;
    subModuleItemCode: number;
    subModuleItemKey: string;
    dataLength: any;
    columnId: number;
    versionNumber: number;
    lookupWindow: string;
    lookupArgument: string;
    filterType: string;
    orderNumber: number;
    isActive: any;
    customElementName: any;
}

export class Option2 {
    optionName: string;
    customDataOptionId: string;
}


export class FormBuilderSaveRO {
    formBuilderId: number;
    documentOwnerPersonId: string;
    moduleItemCode: string;
    moduleSubItemCode: string;
    moduleItemKey: string;
    moduleSubItemKey: string;
    componentId: number;
    componentType: string;
    componentRefId: string;
    componentData?: string;
    programmedElement: any;
    questionnaire: QuestionnaireVO;
    customElement: CustomElementVO;
    files: any[];
}

export class FBConfiguration {
    moduleItemCode: string;
    moduleSubItemCode: string;
    moduleItemKey: string;
    moduleSubItemKey: string;
    documentOwnerPersonId: string;
    formBuilderId: number;
}

export class FBActionEvent {
    action: string;
    actionResponse: SectionComponent| CompUnComp | OPAInstituteResources | OutsideFinRelation | StudentSubordinateEmployee;
    component: SectionComponent | CompUnCompPE | OPAInstituteResourcesPE | OutsideFinRelationPE | StudentSubordinatePE;
}
export interface component {
  formBuilderSectCompId: number
  formBuilderSectionId: number
  formBuilderId: any
  componentTypeCode: string
  componentOrderNumber: number
  componentData: string
  componentRefId: string
  description: string
  headerInstruction: string
  footerInstruction: string
  isActive: string
  updateTimestamp: string
  updateUser: string
  isMandatory: any
  validationMessage: any
  label: any
  componentTypeDescription:any
  }
  
  export interface formHeader {
    formBuilderId: number
    formBuilderNumber: string
    versionNumber: number
    versionStatus: string
    title: string
    description: string
    isActive: string
    createTimestamp: string
    createUser: string
    updateTimestamp: string
    updateUser: string
    usages: any
    sections: any
  }

  export interface NewSection {
    formBuilderSectionId: number
    formBuilderId: number
    sectionName: string
    sectionOrderNumber: number
    businessRuleId: any
    description: string
    helpText: string
    headerInstruction: string
    footerInstruction: string
    isActive: string
    updateTimestamp: string
    updateUser: string
    sectionComponents: any
  }

  export interface ReadSectionComponent {
    formBuilderSectCompId: number
    formBuilderSectionId: number
    formBuilderId: any
    componentTypeCode: string
    componentTypeDescription: any
    componentOrderNumber: number
    componentData: string
    componentRefId: string
    description: string
    headerInstruction: string
    footerInstruction: string
    isActive: string
    updateTimestamp: string
    updateUser: string
    isMandatory: string
    validationMessage: any
    label: any
  }

  export interface FormList {
    formBuilderId: number
    formBuilderNumber: string
    versionNumber: number
    versionStatus: string
    title: any
    description: string
    isActive: string
    updateUser: string
    updateTimestamp: string
  }

  export interface ElementTree {
    componentTypeCode: string
    description: string
    isActive: string
    updateTimestamp: string
    updateUser: string
  }
  
  export interface SectionUpdate {
    formBuilderSectionId: number
    formBuilderId: number
    sectionName: string
    sectionOrderNumber: number
    businessRuleId: any
    description: string
    helpText: string
    headerInstruction: string
    footerInstruction: string
    isActive: string
    updateTimestamp: string
    updateUser: string
    sectionComponents: any
  }
  
  
  export interface ProgramElementList {
    progElementId: number
    progElementNumber: string
    versionNumber: number
    versionStatus: string
    progElementName: string
    description: string
    isActive: string
    updateTimestamp: string
    updateUser: string
  }

  export interface QuestionnaireElementList {
    ACTIVE_QUESTIONNAIRE_VERSION?: string
    QUESTIONNAIRE_LABEL: string
    PENDING_ANSWERED_COUNT?: string
    IS_FINAL: string
    ACTIVE_QUESTIONNAIRE_ID?: string
    PENDING_QUESTIONNAIRE_ID?: string
    QUESTIONNAIRE_NUMBER: number
    PENDING_QUESTIONNAIRE_VERSION?: string
    UPDATE_USER: string
    ACTIVE_ANSWERED_COUNT?: string
    UPDATE_TIMESTAMP: string
  }

  export interface GetQuestionnaire {
    applicableQuestionnaire: any
    questionnaireId: any
    moduleItemKey: any
    moduleSubItemKey: any
    moduleItemCode: any
    moduleSubItemCode: any
    questionnaireAnswerHeaderId: any
    questionnaireAnsAttachmentId: any
    questionnaireCompleteFlag: any
    actionUserId: any
    actionPersonId: any
    actionPersonName: any
    acType: any
    questionnaireName: any
    newQuestionnaireVersion: boolean
    questionEditted: boolean
    questionnaireList: QuestionnaireElementList[]
    questionnaireGroup: QuestionnaireGroup[]
    header: any
    questionnaire: any
    usage: any
    fileName: any
    fileContent: any
    length: any
    remaining: any
    fileTimestamp: any
    contentType: any
    personId: any
    multipartFile: any
    moduleList: any
    isInserted: any
    updateTimestamp: any
    copyModuleItemKey: any
    questionnaireNumbers: any
    lookUpDetails: any
    newQuestionnaireId: any
    moduleSubItemCodes: any[]
    questionnaireBusinessRules: any
    ruleId: any
    rulePassed: any
    questionnaireMode: any
    copyInActiveQuestionAnswers: boolean
  }
  
  export interface QuestionnaireGroup {
    DESCRIPTION: string
    QUEST_GROUP_TYPE_CODE: string
  }

  export interface GetCustomElement {
    customDataElement: any
    customDataElements: CustomDataElement[]
    responseMessage: any
    customDataElementId: any
    customDataTypes: any
    elementOptions: any
    customResponses: any
    moduleCode: any
    customElements: any
    updateUser: any
    updateTimestamp: any
    moduleItemKey: any
    applicableModules: any
    deleteOptions: any
    isDataChange: any
    dataTypeCode: any
    systemLookups: any
    lookUps: any
    elementAnswered: any
    subModuleCode: any
    subModuleItemKey: any
  }
  
  export interface CustomDataElement {
    customElementId: number
    columnId: number
    columnVersionNumber: number
    columnLabel: string
    dataType: string
    customDataTypes: CustomDataTypes
    dataLength?: number
    defaultValue: string
    isLatestVesrion: string
    hasLookup: boolean
    lookupWindow: string
    lookupArgument: string
    isActive: string
    updateUser: string
    updateTimestamp: number
    customElementName: string
    customDataElementUsage: CustomDataElementUsage[]
    acType: any
  }
  
  export interface CustomDataTypes {
    dataTypeCode: string
    description: string
    updateTimestamp: number
    updateUser: string
    isActive: boolean
  }
  
  export interface CustomDataElementUsage {
    customElementUsageId: number
    moduleCode: number
    module: Module
    isRequired: string
    updateUser: string
    updateTimestamp: number
    orderNumber?: number
    acType: any
  }
  
  export interface Module {
    moduleCode: number
    description: string
    updateTimestamp: number
    updateUser: string
    isActive: boolean
  }
  
  export interface ComponentOrder {
    formBuilderSectCompId: number
    formBuilderSectionId: number
    componentOrderNumber: number
  }

  export interface SectionOrder {
    formBuilderSectionId: number
    formBuilderId: string
    sectionOrderNumber: number
  }

  export interface ComponentObject {
    formBuilderSectCompId:  number; 
    componentTypeCode: string;
    componentOrderNumber: number;
    componentData: any; 
    componentRefId: string
    description: string;
    headerInstruction: string;
    footerInstruction: string;
    isActive: any; 
    label: string;
    isMandatory: any;
    validationMessage: string;
  }
    

  export interface UpdateSectionObject{
    formBuilderSectionId: number;
    sectionName: string;
    sectionOrderNumber: number;
    businessRuleId: any;
    description: string;
    helpText: string;
    headerInstruction: string;
    footerInstruction: string;
    isActive: string;
  }

  export interface CreateFormHeader {
    title: string;
    description: string;
  }

  export interface UpdateFormHeaderObject {
    formBuilderId: string;
    title: string;
    description: string;
    isActive: string;
}

export interface  CreateComponentObject {
  formBuilderSectionId: string;
  formBuilderId: string;
  componentTypeCode: any;
  componentOrderNumber: number;
  componentData: string;
  componentRefId: string;
  description: string;
  headerInstruction: string;
  footerInstruction: string;
  isActive: string;
  componentTypeDescription: any;
}

 export interface  FormSectionObject {
  formBuilderId: string;
  sectionName: string;
  sectionOrderNumber: number;
  businessRuleId: any;
  description: string;
  helpText: string;
  headerInstruction: string;
  footerInstruction: string;
  isActive: string;
} 
