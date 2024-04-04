import { Component, OnInit, Input, Output, EventEmitter, OnDestroy } from '@angular/core';
import { Observable, Subscription } from 'rxjs';
import {
    FormSection, SectionComponent, component, ReadSectionComponent, SectionUpdate, ProgramElementList, QuestionnaireElementList, GetQuestionnaire,
    GetCustomElement, CustomDataElement, NewSection ,ComponentObject, UpdateSectionObject
} from '../../../../shared/form-builder-view/form-builder-interface'
import { FormBuilderCreateService } from '../../form-builder-create.service';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';
import { EDITOR_CONFIGURATION } from '../../../.././app-constants'
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';

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

    appAutoCompletedefaultValue: string;
    showSelectedSection: boolean = false;
    programElementList: Array<ProgramElementList> = [];
    questionnaireElementList: Array<QuestionnaireElementList> = [];
    customElementList: Array<CustomDataElement> = [];
    selectedComponent = new SectionComponent;
    selectedSection = new FormSection;
    autoCompleterOption: any = {};
    elementList: any = [];
    appAutoComplete: string = '';
    clearField: String;
    componentHeader: string;
    componentFooter: string;
    public Editor = DecoupledEditor;
    editorConfig = EDITOR_CONFIGURATION;
    $subscriptions: Subscription[] = [];

    constructor(
        public formBuilderService: FormBuilderCreateService) { }


    ngOnInit() {
        this.selectedFormElement();
        this.selectedSectionEvent();
        this.selectedComponent.componentType = ''
        this.initiateAutoSave();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }


    selectedFormElement(): void {
        this.additionInfoComponentEvent.subscribe((data) => {
            setTimeout(() => {
                const searchBox = document.getElementById('searchBox')
                if (searchBox) {
                    document.getElementById('searchBox').focus()
                }
                if (!this.selectedComponent.componentRefId && searchBox) {
                    document.getElementById('searchBox').click()
                }
                if (this.selectedComponent.componentType == 'RT') {
                    document.getElementById('label').focus()
                }
            }, 1000);
            this.showSelectedSection = false;
            //  check if this can be eliminted with just triggering loadComponet();
            this.selectedComponent.componentData = data.componentData;
            this.selectedComponent.componentDescription = data.componentDescription;
            this.selectedComponent.componentFooter = data.componentFooter;
            this.selectedComponent.componentHeader = data.componentHeader;
            this.selectedComponent.componentId = data.componentId;
            this.selectedComponent.componentOrder = data.componentOrder
            this.selectedComponent.componentRefId = data.componentRefId;
            this.selectedComponent.componentType = data.componentType
            this.selectedComponent.sectionId = data.sectionId;
            this.selectedComponent.label = data.label;
            this.selectedComponent.isMandatory = data.isMandatory;
            this.selectedComponent.validationMessage = data.validationMessage;
            this.loadComponent();
        })

    }

    selectedSectionEvent(): void {
        this.additionalInfoSectionEvent.subscribe((data: any) => {
            this.selectedSection.sectionHeader = data.sectionHeader;
            this.selectedSection.sectionFooter = data.sectionFooter;
            this.selectedSection.sectionId = data.sectionId;
            this.selectedSection.sectionName = data.sectionName;
            this.selectedSection.sectionOrder = data.sectionOrder;
            this.showSelectedSection = true;
            this.selectedComponent.componentType = ""
            setTimeout(() => {
                document.getElementById('section-name').focus()
            }, 1000);
            this.loadSection();

        })
    }

    activateSelectedComponent(): void {
        switch (this.selectedComponent.componentType) {
            case 'PE':
                this.getProgramElementList();
                this.appAutoComplete = 'progElementName'
                break;
            case 'CE':
                this.getCustomElementList();
                this.appAutoComplete = 'customElementName'
                break;
            case 'QN':
                this.getQuestionnaireList();
                this.appAutoComplete = 'QUESTIONNAIRE_LABEL'
                break;
        }
    }

    prepareComponentObject() : ComponentObject{
        const prepareComponentObject = {
            "formBuilderSectCompId": this.selectedComponent.componentId,
            "componentTypeCode": this.selectedComponent.componentType,
            "componentOrderNumber": this.selectedComponent.componentOrder,
            "componentData": this.selectedComponent.componentData,
            "componentRefId": this.selectedComponent.componentRefId,
            "description": "desc about the component",
            "headerInstruction": this.selectedComponent.componentHeader,
            "footerInstruction": this.selectedComponent.componentFooter,
            "isActive": "Y",
            "label": this.selectedComponent.label,
            "isMandatory": this.selectedComponent.isMandatory,
            "validationMessage": this.selectedComponent.validationMessage

        }
        return prepareComponentObject;

    }

    updateForComponent(): void {
        this.$subscriptions.push(
            this.formBuilderService.updateComponent(this.prepareComponentObject()).subscribe((data: component) => {
                this.additionalInformationComponent.emit({ componentData: this.prepareComponentObject(), sectionId: this.selectedComponent.sectionId });
                this.formBuilderService.unSavedChange = false;
            })
        );
    }

    prepareSectionObject():UpdateSectionObject {
        const sectionObject = {
            "formBuilderSectionId": this.selectedSection.sectionId,
            "sectionName": this.selectedSection.sectionName,
            "sectionOrderNumber": this.selectedSection.sectionOrder,
            "businessRuleId": null,
            "description": "",
            "helpText": "",
            "headerInstruction": this.selectedSection.sectionHeader,
            "footerInstruction": this.selectedSection.sectionFooter,
            "isActive": "Y"
        }
        return sectionObject;
    }

    updateForSection(): void {
        this.$subscriptions.push(
            this.formBuilderService.updateSection(this.prepareSectionObject()).subscribe((data: SectionUpdate) => {
                // should put this line inside
                this.additionalInformation.emit(this.prepareSectionObject());
                this.formBuilderService.unSavedChange = false;
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
                if (this.selectedComponent.label === null) {
                    this.selectedComponent.label = JSON.parse(JSON.stringify(event.progElementName))
                }
                break;
            case 'CE':
                this.selectedComponent.componentRefId = event.customElementId;
                if (this.selectedComponent.label === null) {
                    this.selectedComponent.label = JSON.parse(JSON.stringify(event.customElementName))
                }
                break;
            case 'QN':
                this.selectedComponent.componentRefId = event.ACTIVE_QUESTIONNAIRE_ID;
                // Intentional comment => if this value need to be changed directly
                // this.selectedComponent.label = event.QUESTIONNAIRE_LABEL
                if (this.selectedComponent.label === null) {
                    this.selectedComponent.label = JSON.parse(JSON.stringify(event.QUESTIONNAIRE_LABEL))
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
                this.selectedComponent.componentHeader = data.headerInstruction;
                this.selectedComponent.componentFooter = data.footerInstruction;
                this.selectedComponent.componentRefId = data.componentRefId;
                this.selectedComponent.componentType = data.componentTypeCode;
                this.activateSelectedComponent();

            })
        )
    }

    loadSection(): void {
        this.$subscriptions.push(
            this.formBuilderService.readSection(this.selectedSection.sectionId).subscribe((data: NewSection) => {
                this.selectedSection.sectionHeader = data.headerInstruction;
                this.selectedSection.sectionFooter = data.footerInstruction;
            })
        )
    }

    public onReady(editor): void {
        editor.ui.getEditableElement().parentElement.insertBefore(
            editor.ui.view.toolbar.element,
            editor.ui.getEditableElement()
        );
    }

    initiateAutoSave(): void {
        this.formBuilderService.autoSaveTrigger$.subscribe((data) => {
            if (data = "COMPONENT") {
                this.updateForComponent();
            }
        })
    }
}

