import { Component, OnInit, Input, Output, EventEmitter, OnDestroy } from '@angular/core';
import { Observable, Subscription } from 'rxjs';
import { FormSection, SectionComponent } from '../../shared/form-builder-view/form-builder-interface';
import { FormBuilderCreateService } from '../../form-builder-create.service';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';
import { EDITOR_CONFIURATION, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../../../../fibi/src/app/app-constants';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Constants } from '../../../../../../../fibi/src/app/admin-modules/custom-data/custom-data.constants';
import { CommonService } from '../../../../common/services/common.service';
import {
    ConfigureCustomElement, ComponentObjects, CustomDataObject, component, ReadSectionComponent, SectionUpdate, ProgramElementList,
    GetQuestionnaire, NewSection, CreateComponentObject, UpdateSectionObject, CustomDataElements, GetCustomElement, QuestionnaireElementList
} from '../../form-builder-create-interface';

@Component({
    selector: 'app-form-addtional-information',
    templateUrl: './form-addtional-information.component.html',
    styleUrls: ['./form-addtional-information.component.scss'],
})
export class FormAddtionalInformationComponent implements OnInit, OnDestroy {
    @Input() additionInfoComponentEvent: Observable<any>;
    @Input() additionalInfoSectionEvent: Observable<any>;
    @Output() additionalInformation: EventEmitter<any> = new EventEmitter();
    @Output() additionalInformationComponent: EventEmitter<any> = new EventEmitter();
    @Output() additionalInformationInitialComponentSave: EventEmitter<any> = new EventEmitter();

    appAutoCompletedefaultValue: string;
    showSelectedSection = false;
    programElementList: Array<ProgramElementList> = [];
    questionnaireElementList: Array<QuestionnaireElementList> = [];
    customElementList: Array<CustomDataElements> = [];
    selectedComponent = new SectionComponent;
    selectedSection = new FormSection;
    autoCompleterOption: any = {};
    elementList: any = [];
    appAutoComplete = '';
    clearField: String;
    componentHeader: string;
    componentFooter: string;
    public Editor = DecoupledEditor;
    editorConfig = EDITOR_CONFIURATION;
    $subscriptions: Subscription[] = [];
    customDataOptions = [];
    customDatalookUpArray: Array<{}>;
    customDataLookUpVlaue = null;
    customDataDefaultValueArray = [];
    customDataDefaultValue = null;
    formBuilderId: string;
    isInitialSave = false;
    customDataElement = new CustomDataObject;
    selectedComponentData: any = {};
    customDataTempOptions = [];
    deletedCustomDataOptions = [];
    formValidation = new Map();

    constructor(
        public formBuilderService: FormBuilderCreateService,
        private commonService: CommonService) { }


    ngOnInit() {
        this.selectedFormElement();
        this.selectedSectionEvent();
        this.selectedComponent.componentType = '';
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }


    selectedFormElement(): void {
        this.additionInfoComponentEvent.subscribe((data) => {
            if (!Object.keys(data).length) {
                this.selectedComponent.componentType = '';
                return;
            }
            this.setFocusForFields();
            this.showSelectedSection = false;
            this.formValidation.clear();
            if (data.tempId && data.tempId.includes('tempId_')) {
                this.isInitialSave = true;
                this.selectedComponent = new SectionComponent();
                this.selectedComponent.componentType = data.componentTypeCode;
                this.selectedComponent.componentTypeDescription = data.description;
                this.selectedComponent.componentData = '';
                this.selectedComponent.componentHeader = '';
                this.selectedComponent.componentFooter = '';
                this.formBuilderService.currentComponentPosition.tempId = data.tempId;
                this.formBuilderService.currentComponentPosition.sectionId = data.sectionId;
                this.formBuilderService.currentComponentPosition.orderNo = data.componentOrderNumber;
                this.selectedComponent.label = null;
                this.selectedComponent.isMandatory = 'N';
                if (['SE', 'NE', 'DE', 'CB', 'RB', 'ES', 'AS', 'SD', 'UD', 'TE'].includes(this.selectedComponent.componentType)) {
                    this.initializeValuesForUnsavedCustomdata(data);
                }
                this.activateSelectedComponent();
            } else {
                this.isInitialSave = false;
                this.selectedComponent.componentId = data.componentId;
                this.loadComponent();
            }
        });

    }

    initializeValuesForUnsavedCustomdata(data) {
        this.selectedComponentData = data;
        this.customDataElement = new CustomDataObject;
        this.customDataElement.lookupArgument = null;
        this.customDataElement.defaultValue = null;
        this.customDataElement.acType = 'I';
        this.customDataElement.isMultiSelectLookup = 'N';
        this.customDataElement.hasLookup = 'N';
        this.loadCustomDataLookup();
        if (['CB', 'RB'].includes(this.selectedComponent.componentType)) {
            this.customDataOptions = [];
            this.deletedCustomDataOptions = [];
            this.addCustomDataOptions();
        }
    }

    setFocusForFields(): void {
        setTimeout(() => {
            const searchBox = document.getElementById('searchBox');
            const customLabel = document.getElementById('Custom-label');
            if (searchBox) {
                searchBox.focus();
            }
            if (!this.selectedComponent.componentRefId && searchBox) {
                searchBox.click();
            }
            if (this.selectedComponent.componentType == 'RT') {
                document.getElementById('label').focus();
            } if (customLabel) {
                customLabel.focus();
            }
        }, 1000);
    }



    componentInitialSave(): void {
        if (this.isFormFieldValidation()) {
            this.$subscriptions.push(
                this.formBuilderService.createComponent(this.prepareComponentinitialSaveObject()).subscribe((data: component) => {
                    this.selectedComponent.componentData = data.componentData;
                    this.selectedComponent.componentDescription = data.componentDescription;
                    this.selectedComponent.componentFooter = data.componentFooter;
                    this.selectedComponent.componentHeader = data.componentHeader;
                    this.selectedComponent.componentId = data.componentId;
                    this.selectedComponent.componentOrder = data.componentOrder;
                    this.selectedComponent.componentRefId = data.componentRefId;
                    this.selectedComponent.componentType = data.componentType;
                    this.selectedComponent.sectionId = data.sectionId;
                    this.selectedComponent.label = data.label;
                    this.selectedComponent.isMandatory = data.isMandatory;
                    this.selectedComponent.validationMessage = data.validationMessage;
                    this.selectedComponent.componentTypeDescription = data.componentTypeDescription;
                    let selectedComponent;
                    selectedComponent = JSON.parse(JSON.stringify(this.selectedComponent));
                    this.additionalInformationInitialComponentSave.emit(selectedComponent);
                    this.isInitialSave = false;
                    this.customDataElement.acType = 'U';
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Configurations Saved successfully.');
                }, err => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Saving Configurations failed.');
                })
            );
        }
    }

    prepareComponentinitialSaveObject(): CreateComponentObject {
        let formSectionId, orderNo;
        if (this.formBuilderService.isComponentOrderChange()) {
            formSectionId = this.formBuilderService.newComponentPosition.sectionId;
            orderNo = this.formBuilderService.newComponentPosition.orderNo;
        } else {
            formSectionId = this.formBuilderService.currentComponentPosition.sectionId;
            orderNo = this.formBuilderService.currentComponentPosition.orderNo;
        }
        const createComponentObject = new CreateComponentObject();
        createComponentObject.sectionId = formSectionId;
        createComponentObject.formBuilderId = this.formBuilderId;
        createComponentObject.componentType = this.selectedComponent.componentType;
        createComponentObject.componentOrder = orderNo;
        createComponentObject.componentData = this.selectedComponent.componentData;
        createComponentObject.componentRefId = this.selectedComponent.componentRefId;
        createComponentObject.description = 'test';
        createComponentObject.componentFooter = this.selectedComponent.componentFooter;
        createComponentObject.componentHeader = this.selectedComponent.componentHeader;
        createComponentObject.isActive = 'Y';
        createComponentObject.componentTypeDescription = this.selectedComponent.componentTypeDescription;
        createComponentObject.label = this.selectedComponent.label;
        createComponentObject.isMandatory = this.selectedComponent.isMandatory;
        createComponentObject.validationMessage = this.selectedComponent.validationMessage;
        return createComponentObject;
    }

    selectedSectionEvent(): void {
        this.additionalInfoSectionEvent.subscribe((data: any) => {
            if (!Object.keys(data).length) {
                this.selectedSection.sectionId = null;
                return;
            }
            this.selectedSection.sectionHeader = data.sectionHeader;
            this.selectedSection.sectionFooter = data.sectionFooter;
            this.selectedSection.sectionId = data.sectionId;
            this.selectedSection.sectionName = data.sectionName;
            this.selectedSection.sectionOrder = data.sectionOrder;
            this.showSelectedSection = true;
            this.selectedComponent.componentType = '';
            setTimeout(() => {
                document.getElementById('section-name').focus();
            }, 1000);
            this.loadSection();

        });
    }

    activateSelectedComponent(): void {
        switch (this.selectedComponent.componentType) {
            case 'PE':
                this.appAutoComplete = 'progElementName';
                this.getProgramElementList();
                break;
            case 'CE':
                this.appAutoComplete = 'customElementName';
                this.getCustomElementList();
                break;
            case 'QN':
                this.appAutoComplete = 'QUESTIONNAIRE_LABEL';
                this.getQuestionnaireList();
                break;
        }
    }

    prepareComponentObject(): ComponentObjects {
        const prepareComponentObject = new ComponentObjects();
        prepareComponentObject.componentId = this.selectedComponent.componentId;
        prepareComponentObject.componentType = this.selectedComponent.componentType;
        prepareComponentObject.componentData = this.selectedComponent.componentData;
        prepareComponentObject.componentRefId = this.selectedComponent.componentRefId;
        prepareComponentObject.description = '';
        prepareComponentObject.componentHeader = this.selectedComponent.componentHeader;
        prepareComponentObject.componentFooter = this.selectedComponent.componentFooter;
        prepareComponentObject.isActive = 'Y';
        prepareComponentObject.label = this.selectedComponent.label;
        prepareComponentObject.isMandatory = this.selectedComponent.isMandatory;
        prepareComponentObject.validationMessage = this.selectedComponent.validationMessage;
        return prepareComponentObject;

    }

    updateFormComponent(): void {
        if (this.isFormFieldValidation()) {
            this.$subscriptions.push(
                this.formBuilderService.updateComponent(this.prepareComponentObject()).subscribe((data: component) => {
                    this.additionalInformationComponent
                        .emit({ componentData: this.prepareComponentObject(), sectionId: this.selectedComponent.sectionId });
                    this.formBuilderService.unSavedChange = false;
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Configurations updated successfully.');
                }, err => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Updating Configurations failed.');
                })
            );
        }
    }

    prepareSectionObject(): UpdateSectionObject {
        const sectionObject = new UpdateSectionObject();
        sectionObject.sectionId = this.selectedSection.sectionId;
        sectionObject.sectionName = this.selectedSection.sectionName;
        sectionObject.sectionOrder = this.selectedSection.sectionOrder;
        sectionObject.sectionBusinessRule = null;
        sectionObject.sectionDescription = '';
        sectionObject.sectionHelpText = '';
        sectionObject.sectionHeader = this.selectedSection.sectionHeader;
        sectionObject.sectionFooter = this.selectedSection.sectionFooter;
        sectionObject.isActive = 'Y';

        return sectionObject;
    }

    updateForSection(): void {
        this.$subscriptions.push(
            this.formBuilderService.updateSection(this.prepareSectionObject()).subscribe((data: SectionUpdate) => {
                this.additionalInformation.emit(this.prepareSectionObject());
                this.formBuilderService.unSavedChange = false;
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Configurations updated successfully.');
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Updating Configurations failed.');
            })
        );

    }

    refrenceId(event): void {
        this.formBuilderService.unSavedChange = true;
        if (!event) {
            this.selectedComponent.componentRefId = '';
            return;
        }
        switch (this.selectedComponent.componentType) {
            case 'PE':
                this.selectedComponent.componentRefId = event.progElementId;
                if (this.selectedComponent.label == null) {
                    this.selectedComponent.label = JSON.parse(JSON.stringify(event.progElementName));
                }
                break;
            case 'CE':
                this.selectedComponent.componentRefId = event.customElementId;
                if (this.selectedComponent.label == null) {
                    this.selectedComponent.label = JSON.parse(JSON.stringify(event.customElementName));
                }
                break;
            case 'QN':
                this.selectedComponent.componentRefId = event.ACTIVE_QUESTIONNAIRE_ID;
                // Intentional comment => if this value need to be changed directly
                // this.selectedComponent.label = event.QUESTIONNAIRE_LABEL
                if (this.selectedComponent.label == null) {
                    this.selectedComponent.label = JSON.parse(JSON.stringify(event.QUESTIONNAIRE_LABEL));
                }
                break;
        }
    }

    setAutoCompleterOptions(): void {
        this.autoCompleterOption = {};
        this.autoCompleterOption.arrayList = this.elementList;
        this.autoCompleterOption.contextField = this.appAutoComplete;
        this.autoCompleterOption.formatString = this.appAutoComplete;
        this.autoCompleterOption.defaultValue = this.appAutoCompletedefaultValue;
        this.autoCompleterOption.filterFields = this.appAutoComplete;

    }

    getProgramElementList(): void {
        this.elementList = [];
        if (!this.programElementList.length) {
            this.$subscriptions.push(
                this.formBuilderService.getProgramElementList().subscribe((data: Array<ProgramElementList>) => {
                    this.programElementList = data;
                    this.elementList = this.programElementList;
                    this.setProgramElementField();
                })
            );
        } else {
            this.elementList = this.programElementList;
            this.setProgramElementField();
        }
    }

    setProgramElementField(): void {
        if (this.selectedComponent.componentRefId) {
            const defaultValue = this.elementList.filter(ele => ele.progElementId == this.selectedComponent.componentRefId);
            this.appAutoCompletedefaultValue = defaultValue[0].progElementName;
        } else {
            this.appAutoCompletedefaultValue = '';
        }
        this.setAutoCompleterOptions();
    }

    getQuestionnaireList(): void {
        this.elementList = [];
        if (!this.questionnaireElementList.length) {
            this.$subscriptions.push(
                this.formBuilderService.getQuestionnaireList().subscribe((data: GetQuestionnaire) => {
                    this.questionnaireElementList = data.questionnaireList;
                    this.elementList = this.questionnaireElementList;
                    this.setQuestionnaireElementField();

                })
            );
        } else {
            this.elementList = this.questionnaireElementList;
            this.setQuestionnaireElementField();
        }
    }

    setQuestionnaireElementField(): void {
        if (this.selectedComponent.componentRefId) {
            const defaultValue = this.elementList.filter(ele => ele.ACTIVE_QUESTIONNAIRE_ID == this.selectedComponent.componentRefId);
            this.appAutoCompletedefaultValue = defaultValue[0].QUESTIONNAIRE_LABEL;
        } else {
            this.appAutoCompletedefaultValue = '';
        }
        this.setAutoCompleterOptions();
    }

    getCustomElementList(): void {
        this.elementList = [];
        if (!this.customElementList.length) {
            this.$subscriptions.push(
                this.formBuilderService.getCustomElementList().subscribe((data: GetCustomElement) => {
                    this.customElementList = data.customDataElements;
                    this.elementList = this.customElementList;
                    this.setCustomElementField();
                })
            );
        } else {
            this.elementList = this.customElementList;
            this.setCustomElementField();
        }
    }

    setCustomElementField(): void {
        if (this.selectedComponent.componentRefId) {
            const defaultValue = this.elementList.filter(ele => ele.customElementId == this.selectedComponent.componentRefId);
            this.appAutoCompletedefaultValue = defaultValue[0].customElementName;
        } else {
            this.appAutoCompletedefaultValue = '';
        }
        this.setAutoCompleterOptions();
    }

    loadComponent(): void {
        this.$subscriptions.push(
            this.formBuilderService.readComponent(this.selectedComponent.componentId).subscribe((data: ReadSectionComponent) => {
                this.selectedComponent.componentHeader = data.componentHeader;
                this.selectedComponent.componentFooter = data.componentFooter;
                this.selectedComponent.componentRefId = data.componentRefId;
                this.selectedComponent.componentType = data.componentType;
                this.selectedComponent.isMandatory = data.isMandatory;
                this.selectedComponent.validationMessage = data.validationMessage;
                this.selectedComponent.label = data.label;
                this.selectedComponent.componentData = data.componentData;
                this.selectedComponent.sectionId = data.sectionId;
                if (['SE', 'NE', 'DE', 'CB', 'RB', 'ES', 'AS', 'SD', 'UD', 'TE'].includes(this.selectedComponent.componentType)) {
                    this.fetchCustomData(this.selectedComponent.componentRefId);
                } else {
                    this.activateSelectedComponent();
                }
            })
        );
    }

    loadSection(): void {
        this.$subscriptions.push(
            this.formBuilderService.readSection(this.selectedSection.sectionId).subscribe((data: NewSection) => {
                this.selectedSection.sectionHeader = data.sectionHeader;
                this.selectedSection.sectionFooter = data.sectionFooter;
            })
        );
    }

    fetchCustomData(componentRefId: string): void {
        this.$subscriptions.push(
            this.formBuilderService.fetchCustomData({ 'customDataElementId': componentRefId }).subscribe((data) => {
                this.customDataElement.dataLength = data.customDataElement.dataLength;
                this.customDataElement.defaultValue = data.customDataElement.defaultValue;
                this.customDataElement.customElementId = data.customDataElement.customElementId;
                this.customDataElement.lookupArgument = data.customDataElement.lookupArgument;
                this.customDataTempOptions = data.elementOptions;
                this.customDataOptions = JSON.parse(JSON.stringify(this.customDataTempOptions));
                this.deletedCustomDataOptions = [];
                this.customDataElement.acType = 'U';
                this.selectedComponentData.componentTypeCode = data.customDataElement.customDataTypes.componentTypeCode;
                this.selectedComponentData.description = data.customDataElement.customDataTypes.description;
                this.selectedComponentData.updateTimestamp = data.customDataElement.customDataTypes.updateTimestamp;
                this.selectedComponentData.updateUser = data.customDataElement.customDataTypes.updateUser;
                this.loadCustomDataLookup();
                this.selectedCustomDataLookup();

            })
        );
    }

    public onReady(editor): void {
        editor.ui.getEditableElement().parentElement.insertBefore(
            editor.ui.view.toolbar.element,
            editor.ui.getEditableElement()
        );
    }

    addCustomDataOptions(): void {
        this.customDataOptions.push({
            'optionName': '',
            'customDataElementsId': this.customDataElement.customElementId || '',
            'updateUser': this.commonService.getCurrentUserDetail('userName'),
            'updateTimestamp': new Date().getTime()
        });
    }

    deleteOptions(index: number): void {
        if (this.customDataOptions.length !== 1) {
            if (this.customDataElement.acType == 'U') {
                this.deletedCustomDataOptions = this.customDataOptions.splice(index, 1);
            } else {
                this.customDataOptions.splice(index, 1);
            }
        }
    }

    loadCustomDataLookup(): void {
        if (['AS', 'ES', 'SD', 'UD'].includes(this.selectedComponent.componentType)) {
            this.$subscriptions.push(
                this.formBuilderService.getSystemLookupByCustomType({ dataTypeCode: this.selectedComponent.componentType })
                    .subscribe((data) => {
                        this.customDatalookUpArray = data.lookUps;
                    })
            );

        }
    }

    selectedCustomDataLookup(): void {
        this.customDataDefaultValueArray = [];
        switch (this.customDataElement.lookupArgument) {
            case 'fibiperson': this.customDataDefaultValueArray = Constants.person; break;
            case 'sponsorName': this.customDataDefaultValueArray = Constants.sponsor; break;
            case 'fibiproposal': this.customDataDefaultValueArray = Constants.proposal; break;
            case 'awardfibi': this.customDataDefaultValueArray = Constants.award; break;
            case 'instituteproposal': this.customDataDefaultValueArray = Constants.instituteproposal; break;
            case 'grantcall_elastic': this.customDataDefaultValueArray = Constants.grantcallElastic; break;
            case 'unitName': this.customDataDefaultValueArray = Constants.leadUnit; break;
            case 'fibiDepartment': this.customDataDefaultValueArray = Constants.department; break;
            case 'fibiOrganization': this.customDataDefaultValueArray = Constants.organization; break;
            case 'fibiCountry': this.customDataDefaultValueArray = Constants.country; break;
            case 'profitCenterName': this.customDataDefaultValueArray = Constants.profitCenter; break;
            case 'grantCodeName': this.customDataDefaultValueArray = Constants.grantCodeName; break;
            case 'costCenterName': this.customDataDefaultValueArray = Constants.costCenter; break;
            case 'fundCenterName': this.customDataDefaultValueArray = Constants.fundCenter; break;
            case 'claimTemplateName': this.customDataDefaultValueArray = Constants.claimTemplate; break;
        }

    }

    setCustomDataTypes(): void {
        this.customDataElement.columnLabel = this.selectedComponent.label;
        this.customDataElement.customDataTypes = {
            componentTypeCode: this.selectedComponentData.componentTypeCode,
            description: this.selectedComponentData.description,
            updateTimestamp: this.selectedComponentData.updateTimestamp,
            updateUser: this.selectedComponentData.updateUser,
            isActive: 'Y'
        };
        this.customDataElement.updateTimestamp = new Date().getTime();
        this.customDataElement.updateUser = this.commonService.getCurrentUserDetail('fullName');
        this.customDataElement.isActive = 'Y';
        this.customDataElement.lookupWindow = '';
        this.customDataElement.customElementName = this.selectedComponent.label;
        this.customDataElement.dataLength = this.customDataElement.dataLength || '';
        this.customDataElement.defaultValue = this.customDataElement.defaultValue || '';
        this.customDataElement.lookupArgument = this.customDataElement.lookupArgument || '';
        this.customDataElement.dataType = this.selectedComponentData.componentTypeCode;
        if (['SD', 'UD'].includes(this.selectedComponent.componentType)) {this.customDataElement.hasLookup = 'Y'; }
    }

    saveCustomData(): void {
        if (this.isFormFieldValidation()) {
            this.setCustomDataTypes();
            const customDataElement = {
                customDataElement: this.customDataElement,
                elementOptions: this.customDataOptions,
                deleteOptions: this.deletedCustomDataOptions
            };
            this.$subscriptions.push(
                this.formBuilderService.configureCustomElement(customDataElement).subscribe((data: ConfigureCustomElement) => {
                    this.selectedComponent.componentRefId = data.customDataElement.customElementId;
                    if (this.customDataElement.acType == 'I') { this.componentInitialSave(); } else { this.updateFormComponent(); }
                }, err => {
                     this.commonService.showToast(HTTP_ERROR_STATUS, `${this.customDataElement.acType == 'I' ? 'Saving Configurations failed.' : 'Updating Configurations failed.'}`);
                })
            );
        }
    }

    saveForComponents(): void {
        if (!['SE', 'NE', 'DE', 'CB', 'RB', 'ES', 'AS', 'SD', 'UD', 'TE'].includes(this.selectedComponent.componentType)) {
            this.componentInitialSave();
        } else {
            this.saveCustomData();
        }
    }

    updateForComponents(): void {
        if (!['SE', 'NE', 'DE', 'CB', 'RB', 'ES', 'AS', 'SD', 'UD', 'TE'].includes(this.selectedComponent.componentType)) {
            this.updateFormComponent();
        } else {
            this.saveCustomData();
        }
    }

    isFormFieldValidation(): boolean {
        this.formValidation.clear();
        if (!this.selectedComponent.label) {
            this.formValidation.set('labelValidation', true);
        }
        if (['PE', 'QN'].includes(this.selectedComponent.componentType) && !this.selectedComponent.componentRefId) {
            this.formValidation.set('componentRefIDValidation', true);
        }
        if (['RB', 'CB'].includes(this.selectedComponent.componentType)) {
            const emptyOptions = this.customDataOptions.find(x => x.optionName == '');
            if (emptyOptions) {
                this.formValidation.set('Optionvalidation', true);
            }
        }
        if (['NE', 'SE'].includes(this.selectedComponent.componentType) && !this.customDataElement.dataLength) {
            this.formValidation.set('dataLengthValidation', true);
        }
        if (['AS', 'UD', 'ES', 'SD'].includes(this.selectedComponent.componentType) && !this.customDataElement.lookupArgument) {
            this.formValidation.set('lookupArgumentValidation', true);
        }
        if (this.formValidation.size > 0) {
            return false;
        }
        return true;

    }

    numberValidation(event: any): void {
        const inputPattern = /^[0-9]+$/;
        const pasted_text = event.clipboardData ? (event.clipboardData).getData('text') : null;
        const inputString = pasted_text ? pasted_text : String.fromCharCode(event.charCode);
        if (!inputPattern.test(inputString)) {
            event.preventDefault();
        } else if (pasted_text) {
            event.preventDefault();
            event.target.value = event.target.value ? event.target.value + pasted_text : pasted_text;
        }
    }


}
