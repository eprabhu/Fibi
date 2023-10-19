export class FormBuilder {
    applicableFormsBuilderIds: number[];
    form: Form;
}

type EventTypes = ['EXTERNAL_SAVE', 'SAVE', 'SAVE_COMPLETE', 'SAVE_COMPLETED', 'CONFIGURATION', 'IS_EDIT_MODE'];

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
}

export class SectionComponent {
    componentId: number;
    sectionId: number;
    componentDescription: any;
    componentType: string;
    componentRefId?: string;
    componentData?: string;
    componentHeader?: string;
    componentFooter: any;
    programmedElement: any;
    questionnaire?: QuestionnaireVO;
    customElement?: CustomElementVO;
    componentOrder: number;
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
