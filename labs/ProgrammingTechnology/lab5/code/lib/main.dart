import 'package:flutter/material.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Лабораторная работа №5',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
      ),
      home: const MyHomePage(),
    );
  }
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({super.key});

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        leading: const Icon(Icons.code),
        title: const Text('Черкасов Александр Андреевич'),
        backgroundColor: Colors.blue,
      ),
      body: Stack(
        children: [
          Positioned(
            top: 20,
            left: 20,
            child: Container(width: 100, height: 100, color: Colors.red),
          ),

          Positioned(
            top: 20,
            right: 20,
            child: Container(width: 80, height: 120, color: Colors.green),
          ),

          Positioned(
            bottom: 20,
            left: 20,
            child: Container(width: 120, height: 80, color: Colors.blue),
          ),

          Positioned(
            bottom: 20,
            right: 20,
            child: Container(width: 90, height: 90, color: Colors.orange),
          ),

          Center(
            child: Container(
              color: Colors.amber[100],
              padding: const EdgeInsets.all(20),
              child: Padding(
                padding: const EdgeInsets.all(16),
                child: Text(
                  'Технологии программирования',
                  style: TextStyle(
                    fontSize: 28,
                    fontWeight: FontWeight.bold,
                    color: Colors.blue[900],
                  ),
                  textAlign: TextAlign.center,
                ),
              ),
            ),
          ),
        ],
      ),
    );
  }
}
