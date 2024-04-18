import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { FormBuilderCreateService } from '../../form-builder-create.service';
import { CreateFormHeader, UpdateFormHeaderObject, formHeader } from 'projects/coi/src/app/shared/form-builder-view/form-builder-interface';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
// import { Route } from '@angular/router';

@Component({
    selector: 'app-form-builder-nav',
    templateUrl: './form-builder-nav.component.html',
    styleUrls: ['./form-builder-nav.component.scss']
})
export class FormBuilderNavComponent implements OnInit, OnDestroy {
    formBuilderId: string;
    editFormTitle: boolean = false;
    formTitle: string;
    $subscriptions: Subscription[] = [];

    constructor(private _route: ActivatedRoute, private _formBuilderService: FormBuilderCreateService,
        private navigation: Router) { }

    ngOnInit() {
        this._route.queryParamMap.subscribe(queryParams => {
            this.formBuilderId = queryParams.get('formBuilderId');
            this.serviceForLoadingForm(this.formBuilderId);
        })

    }

    serviceForLoadingForm(formBuilderId: string): void {
        this.$subscriptions.push(
            this._formBuilderService.getFormDeatails(formBuilderId).subscribe((data: any) => {
                this.formTitle = data.formHeader.title;
            })
        );
    }

    prepareFormHeaderObject(): UpdateFormHeaderObject {
        const formHeaderObject = {
            "formBuilderId": this.formBuilderId,
            "title": this.formTitle,
            "description": "",
            "isActive": "Y"
        }
        return formHeaderObject;
    }

    saveTitle(): void {
        this.$subscriptions.push(
            this._formBuilderService.updateFormHeader(this.prepareFormHeaderObject()).subscribe((data: formHeader) => {
            })
        );
    }

    editTitle(): void {
        setTimeout(() => {
            document.getElementById('edit-Input').focus()
        }, 100);
    }

    navigateToTab(tab): void {
        switch (tab) {
            case '1':
                this.navigation.navigate(['/coi/form-builder-create/form-editor/editor'], { queryParams: { formBuilderId: this.formBuilderId } });
                break;
            case '2':
                this.navigation.navigate(['/coi/form-builder-create/form-editor/integration'], { queryParams: { formBuilderId: this.formBuilderId, title: this.formTitle } });
                break;
            case '3':
                this.navigation.navigate(['/coi/form-builder-create/form-editor/preview'], { queryParams: { formBuilderId: this.formBuilderId } });
                break;
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}
