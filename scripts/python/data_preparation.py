# -*- coding: utf-8 -*-
"""data_preparation.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1Ptkyv9iKiP-VHUt5LixY9tSFh1lPGxVn
"""

# Importamos las bibliotecas necesarias
import pandas as pd  # Para manipulación de datos
import numpy as np   # Para cálculos numéricos
import seaborn as sns  # Para visualizaciones
import matplotlib.pyplot as plt  # Para personalizar gráficos
import plotly.express as px  # Para gráficos interactivos

# Configuración de visualización
plt.style.use('seaborn-v0_8')  # Estilo más moderno
pd.set_option('display.float_format', lambda x: '%.2f' % x)  # Formato de decimales

# Cargamos el dataset
df = pd.read_csv('enhanced_saas_marketing_data.csv')

# Convertimos la columna de fecha a datetime
df['date'] = pd.to_datetime(df['date'])

# Exploración inicial
def explorar_datos(df):
    """
    Función para realizar una exploración inicial del dataset

    Parámetros:
    df: DataFrame de pandas con los datos

    Retorna:
    Dict con información básica del dataset
    """
    resumen = {
        'shape': df.shape,  # Dimensiones del dataset
        'missing_values': df.isnull().sum(),  # Valores faltantes
        'dtypes': df.dtypes,  # Tipos de datos
        'descripcion': df.describe()  # Estadísticas básicas
    }
    return resumen

# Ejecutamos la exploración
resumen_inicial = explorar_datos(df)

# Imprimimos los resultados de forma ordenada
print("📊 Dimensiones del Dataset:")
print(f"Filas: {resumen_inicial['shape'][0]}")
print(f"Columnas: {resumen_inicial['shape'][1]}\\n")

print("🔍 Tipos de Datos:")
print(resumen_inicial['dtypes'])

def limpiar_datos(df):
    """
    Función para limpiar y preparar los datos para el análisis

    Parámetros:
    df: DataFrame original

    Retorna:
    DataFrame limpio y preparado
    """
    # Creamos una copia para no modificar los datos originales
    df_clean = df.copy()

    # 1. Manejo de valores faltantes
    df_clean = df_clean.fillna({
        'revenue': df_clean['revenue'].mean(),
        'costs': df_clean['costs'].mean(),
        'customer_satisfaction': df_clean['customer_satisfaction'].median()
    })

    # 2. Creación de métricas derivadas
    df_clean['gross_margin'] = (df_clean['revenue'] - df_clean['costs']) / df_clean['revenue']
    df_clean['marketing_efficiency'] = df_clean['revenue'] / df_clean['marketing_spend']
    df_clean['clv_cac_ratio'] = df_clean['customer_lifetime_value'] / df_clean['customer_acquisition_cost']

    # 3. Agregamos métricas temporales
    df_clean['year_month'] = df_clean['date'].dt.to_period('M')
    df_clean['quarter'] = df_clean['date'].dt.quarter
    df_clean['year'] = df_clean['date'].dt.year

    return df_clean

# Aplicamos la limpieza
df_limpio = limpiar_datos(df)

# Guardamos los datos limpios
df_limpio.to_csv('Datos_procesados.csv', index=False)