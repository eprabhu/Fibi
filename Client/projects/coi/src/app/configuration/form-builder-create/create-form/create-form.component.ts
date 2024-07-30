import { Component } from '@angular/core';
import { FormBuilderCreateService } from '../form-builder-create.service';
import { ActivatedRoute, Router } from '@angular/router';
import { formHeader } from '../form-builder-create-interface';

@Component({
    selector: 'app-create-form',
    templateUrl: './create-form.component.html',
    styleUrls: ['./create-form.component.scss']
})
export class CreateFormComponent {
    title = '';
    description = '';
    formValidation = new Map();

    constructor(private _formBuilderService: FormBuilderCreateService, private _router: Router, private _activatedRoute: ActivatedRoute) { }

    createFormHeader(): void {
        if (this.isFormFieldValid()) {
            this._formBuilderService.createFormHeader({ 'title': this.title, 'description': this.description })
                .subscribe((data: formHeader) => {
                    this._router.navigate(['../form-editor'],
                        { queryParams: { formBuilderId: data.formBuilderId }, relativeTo: this._activatedRoute });
                });
        }
    }

    isFormFieldValid(): boolean {
        this.formValidation.clear();
        if (this.title === '') {
            this.formValidation.set('titleValidation', true);
        }
        if (this.description === '') {
            this.formValidation.set('descriptionValidation', true);
        }
        if (this.formValidation.size > 0) {
            return false;
        }
        return true;
    }

}
