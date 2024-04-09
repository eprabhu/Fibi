import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { CreateFormComponent } from './create-form.component';
import { Routes, RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../../shared/shared.module';

const routes: Routes = [
    {
        path: '', component: CreateFormComponent,
    }]

@NgModule({
    imports: [
        SharedModule,
        CommonModule,
        RouterModule.forChild(routes),
        FormsModule

    ],
    declarations: [CreateFormComponent]
})
export class CreateFormModule { }
