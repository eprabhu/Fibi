import { Component, OnInit, Output, EventEmitter, OnDestroy } from '@angular/core';
import { CdkDragDrop, moveItemInArray, transferArrayItem } from '@angular/cdk/drag-drop';
import { Form, FormSection, NewSection, SectionComponent, component, ElementTree, CreateComponentObject, FormSectionObject } from '../../../../shared/form-builder-view/form-builder-interface'
import { FormBuilderCreateService } from '../../form-builder-create.service'
import { ActivatedRoute } from '@angular/router';
import { Subject, Subscription } from 'rxjs';
import { deepCloneObject, scrollIntoView } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';

declare const $: any;
@Component({
    selector: 'app-form-editor',
    templateUrl: './form-editor.component.html',
    styleUrls: ['./form-editor.component.scss']
})
export class FormEditorComponent implements OnInit, OnDestroy {
    @Output() additionalInformation: EventEmitter<any> = new EventEmitter();
    origin: Array<ElementTree> = [];
    form = new Form();
    formSection = new FormSection
    sectionComponent = new SectionComponent
    formBuilderId: string;
    sectionArray: Array<FormSection> = [];
    additionInfoComponentEvent: Subject<any> = new Subject<any>();
    additionalInfoSectionEvent: Subject<any> = new Subject<any>();
    formSectionOrderNo: number = 1;
    sectionSortArray: Array<FormSection>;
    lookupSectionComponentType: Array<ElementTree> = [];
    currentlyActiveComponentId: number;
    deleteIndex: number;
    deleteObject: FormSection;
    sectionDelete = false;
    $subscriptions: Subscription[] = [];

    constructor(
        private _formBuilderService: FormBuilderCreateService,
        private _route: ActivatedRoute) { }

    ngOnInit() {
        this._route.queryParamMap.subscribe(queryParams => {
            this.formBuilderId = queryParams.get('formBuilderId');
            if (this.formBuilderId) {
                this.serviceForLoadingForm(this.formBuilderId);
            }
        });
    }
    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    drop(event: CdkDragDrop<string[]>): void {
        // make it false to prevent drag
        if (this._formBuilderService.unSavedChange) {
            this._formBuilderService.initiateAutoSave("COMPONENT")
        }
        if (event.previousContainer === event.container) {
            moveItemInArray(event.container.data, event.previousIndex, event.currentIndex);
        } else {
            transferArrayItem(event.previousContainer.data,
                event.container.data,
                event.previousIndex,
                event.currentIndex);
        }
        this.setComponent(event)
        this.origin = deepCloneObject(this.lookupSectionComponentType);
        setTimeout(() => {
            if (this.currentlyActiveComponentId) {
                this.componentBorder(this.currentlyActiveComponentId);
            }
        }, 1000);
    }

    // Emit component related data to Additional information Component
    emitComponentData(item: SectionComponent): void {
        this.currentlyActiveComponentId = item.componentId;
        if (this._formBuilderService.unSavedChange) {
            this._formBuilderService.initiateAutoSave("COMPONENT");
            setTimeout(() => {
                this.componentBorder(this.currentlyActiveComponentId);
                this.additionInfoComponentEvent.next(item);
            }, 1000);
        } else {
            this.componentBorder(this.currentlyActiveComponentId);
            this.additionInfoComponentEvent.next(item);
        }
    }

    createNewSection(): void {
        this.$subscriptions.push(
            this._formBuilderService.createFormSection(this.createNewSectionObject()).subscribe((data: NewSection) => {
                this.formSection.sectionId = data.formBuilderSectionId;
                this.formSection.sectionName = data.sectionName;
                this.formSection.sectionDescription = data.description;
                this.formSection.sectionHeader = data.headerInstruction;
                this.formSection.sectionFooter = data.footerInstruction;
                this.formSection.sectionHelpText = data.helpText;
                this.formSection.sectionBusinessRule = data.businessRuleId;
                this.formSection.sectionOrder = data.sectionOrderNumber
                this.formSection.sectionComponent = []
                const formSection = deepCloneObject(this.formSection);
                this.sectionArray.push(formSection);
            })
        )
        setTimeout(() => {
            this.scrollIdIntoView(this.formSection.sectionId);
        }, 1000);
    }

    serviceForLoadingForm(formBuilderId: string): void {
        this.$subscriptions.push(
            this._formBuilderService.getFormDeatails(formBuilderId).subscribe((data: any) => {
                this.lookupSectionComponentType = data.lookupSectionComponentType;
                this.origin = deepCloneObject(this.lookupSectionComponentType);
                const sections = data.formHeader.sections;
                sections.forEach(ele => {
                    this.formSection.sectionId = ele.formBuilderSectionId;
                    this.formSection.sectionName = ele.sectionName;
                    this.formSection.sectionDescription = ele.description;
                    this.formSection.sectionHeader = ele.headerInstruction;
                    this.formSection.sectionFooter = ele.footerInstruction;
                    this.formSection.sectionHelpText = ele.helpText;
                    this.formSection.sectionBusinessRule = ele.businessRuleId;
                    this.formSection.sectionOrder = ele.sectionOrderNumber;
                    this.formSection.sectionComponent = (ele.sectionComponents === null) ? [] : this.SetSectionComponentOnLoad(ele.sectionComponents);
                    const formSection = deepCloneObject(this.formSection);
                    this.sectionArray.push(formSection)
                    // deepCloneObject is used because while adding section obj to array all obj  will have same refernce and this 
                    // will cause them to misbehave.

                });
                if (!this.sectionArray.length) {
                    this.createNewSection();
                    setTimeout(() => {
                        document.getElementById('first-Section').click();
                    }, 1000);
                }
            })
        )
    }

    // For loading section components
    SetSectionComponentOnLoad(sectionComponents: Array<component>) {
        const sectionComponent = [];
        sectionComponents.forEach(element => {
            this.sectionComponent.componentData = element.componentData;
            this.sectionComponent.componentDescription = element.description;
            this.sectionComponent.componentFooter = element.footerInstruction;
            this.sectionComponent.componentHeader = element.headerInstruction;
            this.sectionComponent.componentId = element.formBuilderSectCompId;
            this.sectionComponent.componentOrder = element.componentOrderNumber
            this.sectionComponent.componentRefId = element.componentRefId;
            this.sectionComponent.componentType = element.componentTypeCode
            this.sectionComponent.sectionId = element.formBuilderSectionId
            this.sectionComponent.label = element.label;
            this.sectionComponent.isMandatory = element.isMandatory;
            this.sectionComponent.validationMessage = element.validationMessage;
            this.sectionComponent.componentTypeDescription = element.componentTypeDescription;
            sectionComponent.push(deepCloneObject(this.sectionComponent));
        });
        return sectionComponent;
    }

    createNewSectionObject(): FormSectionObject {
        if (this.sectionArray.length) {
            this.formSectionOrderNo = this.sectionArray[this.sectionArray.length - 1].sectionOrder + 1;
        }
        const formSectionObject =
        {
            "formBuilderId": this.formBuilderId,
            "sectionName": "New Section",
            "sectionOrderNumber": this.formSectionOrderNo,
            "businessRuleId": null,
            "description": "Test",
            "helpText": "Help Text 1",
            "headerInstruction": "",
            "footerInstruction": "",
            "isActive": "y"
        }
        return formSectionObject;
    }

    // This function sets requied values for adding a component from the event of drop function provide by the Angular Drag & Drop cdk.
    setComponent(event): void {
        const formSectionId = event.container.id;
        // container is an array that  receives the dragged component
        const container = event.container.data;
        // index of array at which component was droped.
        const containerindex = event.currentIndex;

        const previousContainer = event.previousContainer;
        // dropContainer is the Section that  receives the dragged component
        const dropContainer = this.sectionArray.find(obj => obj['sectionId'] == formSectionId);
        this.createcomponents(formSectionId, containerindex, container, dropContainer, previousContainer);

    }

    createcomponents(formSectionId: string, containerindex: number, container, dropContainer, previousContainer): void {
        if (previousContainer.id.includes("cdk-drop-list-")) {
            //  if condition is satisfied for an element dragged from form-element tree,
            //  and not satisfied for interdragged components ie, components dragged b/w sections or  with in the section.
            this.$subscriptions.push(
                this._formBuilderService.createComponent(this.prepareComponentObject(formSectionId, containerindex, container)).subscribe((data: component) => {
                    this.sectionComponent.componentData = data.componentData;
                    this.sectionComponent.componentDescription = data.description;
                    this.sectionComponent.componentFooter = data.footerInstruction;
                    this.sectionComponent.componentHeader = data.headerInstruction;
                    this.sectionComponent.componentId = data.formBuilderSectCompId;
                    this.sectionComponent.componentOrder = data.componentOrderNumber
                    this.sectionComponent.componentRefId = data.componentRefId;
                    this.sectionComponent.componentType = data.componentTypeCode
                    this.sectionComponent.sectionId = data.formBuilderSectionId
                    this.sectionComponent.label = data.label;
                    this.sectionComponent.isMandatory = data.isMandatory;
                    this.sectionComponent.validationMessage = data.validationMessage;
                    this.sectionComponent.componentTypeDescription = data.componentTypeDescription;
                    const sectionComponent = deepCloneObject(this.sectionComponent);
                    dropContainer.sectionComponent[containerindex] = sectionComponent;
                    this.updatePositionOfAllComponentsInSection(container, dropContainer)
                })
            )
        } else {
            this.updatePositionOfAllComponentsInSection(container, dropContainer)
        }
    }

    prepareComponentObject(formSectionId: string, containerindex: number, container, componentRefId = ""): CreateComponentObject {
        const createComponentObject = {
            "formBuilderSectionId": formSectionId,
            "formBuilderId": this.formBuilderId,
            "componentTypeCode": container[containerindex].componentTypeCode,
            "componentOrderNumber": containerindex,
            "componentData": "",
            "componentRefId": componentRefId,
            "description": "",
            "headerInstruction": "",
            "footerInstruction": "",
            "isActive": "Y",
            "componentTypeDescription": container[containerindex].description
        }
        return createComponentObject;
    }

    updatePositionOfAllComponentsInSection(container, dropContainer): void {
        container.forEach((element, index) => {
            this.$subscriptions.push(
                this._formBuilderService.componentOrder([{
                    "formBuilderSectCompId": element.componentId,
                    "formBuilderSectionId": dropContainer.sectionId,
                    "componentOrderNumber": index
                }]).subscribe((data) => {
                })
            )
        }
        );
    }

    confirmComponentDelete(index: number, deleteObject: FormSection): void {
        this.sectionDelete = false;
        $('#deleteConfirmationModal').modal('show');
        this.deleteIndex = index;
        this.deleteObject = deleteObject;
    }

    deleteComponent(): void {
        const indexTobeDeleted = this.deleteObject.sectionComponent[this.deleteIndex].componentId;
        this.$subscriptions.push(
            this._formBuilderService.deleteComponent(indexTobeDeleted).subscribe((data) => {
                this.deleteObject.sectionComponent.splice(this.deleteIndex, 1);
                this.additionInfoComponentEvent.next({})
            })
        )
    }

    sectionSort(): void {
        $('#actionConfirmationModal').modal('show');
        this.sectionSortArray = deepCloneObject(this.sectionArray);
    }

    dropForSort(event: CdkDragDrop<string[]>) {
        moveItemInArray(this.sectionSortArray, event.previousIndex, event.currentIndex);
    }

    sectionSortService(): void {
        this.sectionSortArray.forEach((element, index) => {
            this.$subscriptions.push(
                this._formBuilderService.sectionOrder([{
                    "formBuilderSectionId": element.sectionId,
                    "formBuilderId": this.formBuilderId,
                    "sectionOrderNumber": index + 1,
                }]).subscribe((data) => {
                    this.sectionArray = this.sectionSortArray;
                })
            )
        });
    }

    cancelSectionSort(): void {
        this.sectionSortArray = [];
    }


    emitSectionData(event): void {
        this.sectionBorder(event.sectionId);
        this.additionalInfoSectionEvent.next(event);
    }

    sectionUpdate(event): void {
        let selectedSection: any;
        selectedSection = this.sectionArray.filter(ele => ele.sectionId == event.formBuilderSectionId);
        selectedSection[0].sectionName = event.sectionName;
    }

    componentUpdate(event): void {
        let selectedComponent: any;
        for (const element of this.sectionArray) {
            selectedComponent = element.sectionComponent.find(ele => ele.componentId === event.componentData.formBuilderSectCompId);
            if (selectedComponent) {
                selectedComponent.label = event.componentData.label;
                selectedComponent.isMandatory = event.componentData.isMandatory;
                selectedComponent.validationMessage = event.componentData.validationMessage;
                selectedComponent.componentData = event.componentData.componentData;
                return;
            }
        }
    }

    deleteSectionConfirmation(deleteObject: FormSection, sectionArrayIndex: number): void {
        this.sectionDelete = true;
        $('#deleteConfirmationModal').modal('show');
        this.deleteObject = deleteObject;
        this.deleteIndex = sectionArrayIndex;
    }

    deleteSection(): void {
        this.sectionBorder(this.deleteObject.sectionId);
        this.$subscriptions.push(
            this._formBuilderService.deleteSection(this.deleteObject.sectionId).subscribe((data) => {
                this.sectionArray.splice(this.deleteIndex, 1);
                this.additionalInfoSectionEvent.next({});
            })
        );
    }

    scrollIdIntoView(id): void {
        scrollIntoView(id);
    }

    sectionBorder(selectionId: number): void {
        if (document.getElementsByClassName('border-for-section-selection')[0]) {
            document.getElementsByClassName('border-for-section-selection')[0].classList.remove('border-for-section-selection');
        }
        if (document.getElementsByClassName('clicked')[0]) {
            document.getElementsByClassName('clicked')[0].classList.remove('clicked');
        }
        const backgroundColor = document.getElementById(`section-card-${selectionId}`).classList.add('border-for-section-selection');
    }

    componentBorder(componentId: number): void {
        if (document.getElementsByClassName('clicked')[0]) {
            document.getElementsByClassName('clicked')[0].classList.remove('clicked');
        }
        if (document.getElementsByClassName('border-for-section-selection')[0]) {
            document.getElementsByClassName('border-for-section-selection')[0].classList.remove('border-for-section-selection');
        }
        const backgroundColor = document.getElementById(`field-box-${componentId}`).classList.add('clicked');
    }

}
