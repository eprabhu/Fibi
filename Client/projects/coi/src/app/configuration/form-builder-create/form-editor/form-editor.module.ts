import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilderNavComponent } from '../form-editor/form-builder-nav/form-builder-nav.component';
import { FormIntegrationComponent } from '../form-editor/form-integration/form-integration.component';
import { FormEditorComponent } from '../form-editor/form-editor/form-editor.component';
import { FormAddtionalInformationComponent } from '../form-editor/form-addtional-information/form-addtional-information.component';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { Routes, RouterModule } from '@angular/router';
import { OpaService } from '../../../opa/services/opa.service';
import { DataStoreService } from '../../../opa/services/data-store.service';
import { SharedModule } from '../../../shared/shared.module';
import { FormsModule } from '@angular/forms';

const routes: Routes = [
    {
        path: '', component: FormBuilderNavComponent,
        children: [
            { path: '', redirectTo: 'editor', pathMatch: 'full' },
            { path: 'editor', component: FormEditorComponent },
            { path: 'preview', loadChildren: () => import('./../../../opa/form/form.module').then(m => m.FormModule) },
            { path: 'integration', component: FormIntegrationComponent },
        ]
    }]

@NgModule({
    imports: [
        CommonModule,
        DragDropModule,
        RouterModule.forChild(routes),
        SharedModule,
        FormsModule

    ],
    declarations: [
        FormBuilderNavComponent,
        FormIntegrationComponent,
        FormEditorComponent,
        FormAddtionalInformationComponent,
    ],
    providers: [OpaService, DataStoreService]

})
export class FormEditorModule { }
