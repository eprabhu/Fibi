export class FormBuilder {
    applicableFormsBuilderIds: number[];
    primaryForm: PrimaryForm;
}

export class PrimaryForm {
    formBuilderId: number;
    formBuilderNumber: string;
    formName: string;
    formSections: FormSection[] = [];
}

export class FormSection {
    sectionId: number;
    sectionName: string;
    sectionOrder: number;
    sectionDescription: string;
    sectionBusinessRule: any;
    sectionHelpText: string;
    sectionHeader: string;
    sectionFooter: string;
    sectionComponent: SectionComponent[];
}

export class SectionComponent {
    componentId: number;
    sectionId: number;
    componentDescription: any;
    componentType: string;
    componentRefId?: string;
    componentData: string;
    componentHeader: string;
    componentFooter: string;
    programmedElement: any;
    questionnaire?: QuestionnaireVO;
    customElement?: CustomElement;
    componentOrder: number;
}

export class QuestionnaireVO {
    applicableQuestionnaire: any;
    questionnaireId: number;
    moduleItemKey: any;
    moduleSubItemKey: any;
    moduleItemCode: any;
    moduleSubItemCode: any;
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
    DESCRIPTION?: string;
    SORT_ORDER: number;
    QUESTION_ID: number;
    ANSWERS: any;
    ANSWER_TYPE: string;
    NO_OF_ANSWERS: number;
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
    OPTION_NUMBER: any;
    QUESTION_ID: number;
    OPTION_LABEL: string;
    REQUIRE_EXPLANATION: string;
}

export class CustomElement {
    customDataElement: CustomDataElement;
    customDataElements: any;
    responseMessage: any;
    customDataElementId: number;
    customDataTypes: any;
    elementOptions: any[];
    customResponses: any;
    moduleCode: any;
    customElements: any;
    updateUser: any;
    updateTimestamp: any;
    moduleItemKey: any;
    applicableModules: any;
    deleteOptions: any;
    isDataChange: any;
    dataTypeCode: any;
    systemLookups: any;
    lookUps: any;
    elementAnswered: boolean;
}

export class CustomDataElement {
    customElementId: number;
    columnId: number;
    columnVersionNumber: number;
    columnLabel: string;
    dataType: string;
    customDataTypes: CustomDataTypes;
    dataLength: any;
    defaultValue: string;
    isLatestVesrion: string;
    hasLookup: boolean;
    lookupWindow: string;
    lookupArgument: string;
    isActive: string;
    updateUser: string;
    updateTimestamp: string;
    customElementName: string;
    customDataElementUsage: CustomDataElementUsage[];
    acType: any;
}

export class CustomDataTypes {
    dataTypeCode: string;
    description: string;
    updateTimestamp: string;
    updateUser: string;
    isActive: boolean;
}

export class CustomDataElementUsage {
    customElementUsageId: number;
    moduleCode: number;
    module: Module;
    isRequired: string;
    updateUser: string;
    updateTimestamp: string;
    orderNumber: any;
    acType: string;
}

export class Module {
    moduleCode: number;
    description: string;
    updateTimestamp: string;
    updateUser: string;
    isActive: boolean;
}
